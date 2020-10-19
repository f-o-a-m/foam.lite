module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import DApp.Message (DAppMessage(..), packDAppMessage, parseDAppMessage)
import DApp.Relay (UnsignedRelayedMessage(..), UnsignedRelayedTransfer(..), packSignedRelayedMessage, packSignedRelayedTransfer, signRelayedMessage, signRelayedTransfer)
import Data.ByteString (ByteString, unsafeFreeze)
import Data.Either (Either(..), fromRight, note)
import Data.EitherR (fmapL)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String.Regex as Regex
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (error)
import Network.Ethereum.Core.BigNumber (BigNumber, decimal, hexadecimal, parseBigNumber)
import Network.Ethereum.Core.HexString (HexString, fromByteString, mkHexString, toByteString)
import Network.Ethereum.Core.Signatures (Address, PrivateKey, generatePrivateKey, mkAddress, mkPrivateKey, privateToAddress)
import Network.Ethereum.Web3 (class KnownSize, DLProxy, UIntN, sizeVal, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (S128, S32, s128, s32)
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Process as NP
import Node.Stream as NS
import Options.Applicative (CommandFields, Mod, Parser, ReadM, argument, command, eitherReader, execParser, flag', fullDesc, header, help, helper, info, long, metavar, number, option, progDesc, readerError, short, strOption, subparser, (<**>))
import Partial.Unsafe (unsafePartialBecause)
import Record as Record

data Subcommand = SignTransfer SignTransferOptions
                | SignMint SignMintOptions
                | Decode DecodeOptions
                | GeneratePrivateKey
                | PrivateToAddress PrivateKey

data DataSource = Stdin | File FilePath | Raw ByteString | SuppliedDAppMessage DAppMessage

readFileDataSource :: ReadM DataSource
readFileDataSource = eitherReader (Right <<< match)
  where match "-" = Stdin
        match x = File x

readEncodedDAppMessage :: ReadM DAppMessage
readEncodedDAppMessage = eitherReader (fmapL show <<< parseDAppMessage)

parseDataSource :: Parser DataSource
parseDataSource = (parseStdinFlag <|> parseFileFlag <|> parseHexDataFlag <|> parseEncodedDAppMessage <|> parseSuppliedDAppMessage)
  where parseStdinFlag = flag' Stdin (long "stdin" <> short 's' <> help "read minting data from stdin")
        parseFileFlag = option readFileDataSource (long "file" <> short 'f' <> metavar "file" <> help "read minting data from a file (`-f -` is also stdin)")
        parseHexDataFlag = option (Raw <<< toByteString <$> readHexString) (long "hex" <> short 'x' <> metavar "HEX" <> help "read minting data from a HexString")
        parseEncodedDAppMessage = option (SuppliedDAppMessage <$> readEncodedDAppMessage) (long "message" <> short 'm' <> metavar "DAPPMSG" <> help "use an encoded DAppMessage")
        parseSuppliedDAppMessage = SuppliedDAppMessage <$> (parseLocationWithArbitraryDAppMessage <|> (Location <$> parseLocationDAppMessage)<|> (ArbitraryString <$> parseArbitraryDAppMessage))
        parseArbitraryDAppMessage = (strOption (long "arb" <> short 'a' <> help "make an arbitrary-string DAppMessage"))
        parseLocationDAppMessage = ado
          lat <- option number (long "lat" <> metavar "LAT" <> help "supply a latitude to the DAppMessage")
          lon <- option number (long "lon" <> metavar "LON" <> help "supply a longitude to the DAppMessage")
          in { lat, lon }
        parseLocationWithArbitraryDAppMessage = ado
          { lat, lon } <- parseLocationDAppMessage
          arbStr <- parseArbitraryDAppMessage
          in LocationWithArbitrary { arbStr, lat, lon }


newtype SignTransferOptions =
  SignTransferOptions { privateKey :: PrivateKey
                      , nonce :: UIntN S32
                      , feeAmount :: UIntN S128
                      , tokenID :: UIntN S32
                      , destination :: Address
                      }
newtype SignMintOptions = 
  SignMintOptions { privateKey :: PrivateKey 
                  , nonce :: UIntN S32
                  , feeAmount :: UIntN S128
                  , dataSource :: DataSource
                  }

newtype DecodeOptions =
  DecodeOptions { }

readHexString :: ReadM HexString
readHexString = eitherReader (note "Couldn't make a valid HexString from the supplied value" <<< mkHexString)

readAddress :: ReadM Address
readAddress = do
  hex <- readHexString
  fromMaybe (readerError $ "Couldn't make a valid Address from the supplied HexString") (pure <$> mkAddress hex)

readPrivateKey :: ReadM PrivateKey
readPrivateKey = do
  hex <- readHexString 
  fromMaybe (readerError $ "Couldn't make a valid PrivateKey from the supplied HexString") (pure <$> mkPrivateKey hex)

readBigNumber :: ReadM BigNumber
readBigNumber = eitherReader (note "Couldn't make a valid BigNumber from the supplied value" <<< parseBigNumber')
  where parseBigNumber' s =
          if hasHex s then parseBigNumber hexadecimal s else parseBigNumber decimal s
        hasHex s = isJust $ Regex.search hexRegex s
        hexRegex = unsafePartialBecause "we're compiling a known-good regex" fromRight $ Regex.regex "[XxA-Fa-f]" $ Regex.parseFlags "g"

readUIntN :: forall s. KnownSize s => DLProxy s -> ReadM (UIntN s)
readUIntN size = do
  bn <- readBigNumber
  fromMaybe (readerError $ "Couldn't fit BigNumber into a uint" <> show (sizeVal size)) (pure <$> uIntNFromBigNumber size bn)

mkSubcommand :: forall a. String -> String -> Parser a -> Mod CommandFields a
mkSubcommand sc desc parse = command sc subparserInfo
  where subparserInfo = info (parse <**> helper) commandDesc
        commandDesc = (fullDesc <> progDesc desc <> header ("foam5g-helper-" <> sc <> " - " <> desc))

parseSignTransferOptions :: Parser SignTransferOptions
parseSignTransferOptions = ado
    privateKey <- option readPrivateKey (long "private-key" <> short 'p' <> metavar "HEX" <> help "private key to sign with")
    nonce <- option (readUIntN s32) (long "nonce" <> short 'n' <> metavar "BIGNUM" <> help "nonce to use for message")
    feeAmount <- option (readUIntN s128) (long "fee-amount" <> short 'f' <> metavar "BIGNUM" <> help "fee amount to pay relayer")
    tokenID <- option (readUIntN s32) (long "token-id" <> short 'i' <> metavar "BIGNUM" <> help "token ID to transfer")
    destination <- option readAddress (long "destination" <> short 'd' <> metavar "ADDRESS" <> help "destination address of token")
    in SignTransferOptions { privateKey, nonce, feeAmount, tokenID, destination }


parseSignMintOptions :: Parser SignMintOptions
parseSignMintOptions = ado
    privateKey <- option readPrivateKey (long "private-key" <> short 'p' <> metavar "HEX" <> help "private key to sign with")
    nonce <- option (readUIntN s32) (long "nonce" <> short 'n' <> metavar "BIGNUM" <> help "nonce to use for message")
    feeAmount <- option (readUIntN s128) (long "fee-amount" <> short 'f' <> metavar "BIGNUM" <> help "fee amount to pay relayer")
    dataSource <- parseDataSource
    in SignMintOptions { privateKey, nonce, feeAmount, dataSource }

parseDecodeOptions :: Parser DecodeOptions
parseDecodeOptions = pure $ DecodeOptions {}

parseSubcommand :: Parser Subcommand
parseSubcommand = subparser 
                ( mkSubcommand "sign-transfer" "Sign a relayable transfer" (SignTransfer <$> parseSignTransferOptions)
               <> mkSubcommand "sign-mint" "Sign a relayable mint" (SignMint <$> parseSignMintOptions)
               <> mkSubcommand "decode" "Decode a relayable message" (Decode <$> parseDecodeOptions)
               <> mkSubcommand "generate-private-key" "Generate a private key to sign with" (pure GeneratePrivateKey)
               <> mkSubcommand "private-to-address" "Convert a private key to an Ethereum Address" (PrivateToAddress <$> (argument readPrivateKey $ metavar "HEX"))
                )

main :: Effect Unit
main = runHelper =<< execParser opts
  where opts = info (parseSubcommand <**> helper)
          (  fullDesc
          <> progDesc "Helpers for the FOAM5G App"
          <> header "foam5g-helper - a reference for the rest of us" 
          )

readDataSource :: DataSource -> Effect ByteString
readDataSource Stdin = maybe (throwError $ error "Couldn't read anything from `stdin`")  (pure <<< unsafeFreeze) =<< NS.read NP.stdin Nothing
readDataSource (File fp) = unsafeFreeze <$> FS.readFile fp
readDataSource (Raw bs) = pure bs
readDataSource (SuppliedDAppMessage dm) = pure $ packDAppMessage dm

runHelper :: Subcommand -> Effect Unit
runHelper (SignTransfer (SignTransferOptions t)) = do
  let unsigned = UnsignedRelayedTransfer (Record.delete (SProxy :: SProxy "privateKey") t)
      signed = signRelayedTransfer t.privateKey unsigned
      packed = packSignedRelayedTransfer signed
      hex = fromByteString packed
  Console.log (show hex)
runHelper (SignMint (SignMintOptions { privateKey, nonce, feeAmount, dataSource })) = do
  tokenData <- readDataSource dataSource
  let unsigned = UnsignedRelayedMessage { nonce, feeAmount, tokenData }
      signed = signRelayedMessage privateKey unsigned
      packed = packSignedRelayedMessage signed
      hex = fromByteString packed
  Console.log (show hex)
runHelper (Decode _) = Console.log "todo"
runHelper GeneratePrivateKey = Console.log <<< show =<< generatePrivateKey
runHelper (PrivateToAddress private) = Console.log <<< show $ privateToAddress private
