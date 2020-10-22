module Main where

import Prelude

import Affjax as AF
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Chanterelle.Internal.Artifact (_Deployed, _address, _network, readArtifact)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Utils (pollTransactionReceipt)
import Contracts.FungibleToken as FT
import Control.Alt ((<|>))
import Control.Error.Util (hush)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask)
import DApp.Message (DAppMessage(..), packDAppMessage, parseDAppMessage)
import DApp.Relay (UnsignedRelayedMessage(..), UnsignedRelayedTransfer(..), decodePackedMessage, interpretDecodedMessage, packSignedRelayedMessage, packSignedRelayedTransfer, signRelayedMessage, signRelayedTransfer)
import DApp.Util (makeTxOpts, signApprovalTx, signApprovalTx')
import Data.Argonaut (encodeJson)
import Data.Argonaut.Core (stringifyWithIndent)
import Data.ByteString (ByteString, fromUTF8, unsafeFreeze)
import Data.Either (Either(..), either, fromRight, note)
import Data.EitherR (fmapL)
import Data.Int as Int
import Data.Lens (_Just, (^?), (?~), (.~))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String.Regex as Regex
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Exception (error)
import Network.Ethereum.Core.BigNumber (BigNumber, decimal, embed, hexadecimal, parseBigNumber)
import Network.Ethereum.Core.HexString (HexString, fromByteString, mkHexString, toByteString, unHex)
import Network.Ethereum.Core.Signatures (Address, ChainId(..), PrivateKey, generatePrivateKey, mkAddress, mkPrivateKey, privateToAddress)
import Network.Ethereum.Web3 (class KnownSize, ChainCursor(..), DLProxy, UIntN, Web3, Web3Error, _gas, _gasPrice, _nonce, httpProvider, runWeb3, sizeVal, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3.Api (eth_getTransactionCount, eth_sendRawTransaction, net_version)
import Network.Ethereum.Web3.Solidity.Sizes (S128, S256, S32, s128, s256, s32)
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Process (exit)
import Node.Process as NP
import Node.Stream as NS
import Options.Applicative (CommandFields, Mod, Parser, ReadM, argument, command, eitherReader, execParser, flag', fullDesc, header, help, helper, info, int, long, metavar, number, option, progDesc, readerError, short, strOption, subparser, switch, (<**>))
import Partial.Unsafe (unsafePartialBecause)

data Subcommand = SignTransfer SignTransferOptions
                | SignMint SignMintOptions
                | Decode DecodeOptions
                | GeneratePrivateKey
                | PrivateToAddress PrivateKey
                | ApproveFT ApproveFTOptions

data DataSource = Stdin | File FilePath | Raw ByteString
data DataSource' = DataSource DataSource | SuppliedDAppMessage DAppMessage

readFileDataSource :: ReadM DataSource
readFileDataSource = eitherReader (Right <<< match)
  where match "-" = Stdin
        match x = File x

readEncodedDAppMessage :: ReadM DAppMessage
readEncodedDAppMessage = eitherReader (fmapL show <<< parseDAppMessage)

maybeParser :: forall a. Parser a -> Parser (Maybe a)
maybeParser p = (Just <$> p) <|> (pure Nothing)

parseTransmitEndpoint :: Parser (Maybe String)
parseTransmitEndpoint = maybeParser $ strOption (long "transmit" <> short 'X' <> metavar "URL" <> help "Post signed transaction to a transmission endpoint")

parseDataSource :: String -> Parser DataSource
parseDataSource dataType = parseStdinFlag <|> parseFileFlag <|> parseHexDataFlag
  where parseStdinFlag = flag' Stdin (long "stdin" <> short 's' <> help ("read " <> dataType <> " data from stdin"))
        parseFileFlag = option readFileDataSource (long "file" <> short 'f' <> metavar "file" <> help ("read " <> dataType <> " data from a file (`-f -` is also stdin)"))
        parseHexDataFlag = option (Raw <<< toByteString <$> readHexString) (long "hex" <> short 'x' <> metavar "HEX" <> help ("read " <> dataType <> " data from a HexString"))

parseDataSource' :: Parser DataSource'
parseDataSource' = (DataSource <$> parseDataSource "minting") <|> parseEncodedDAppMessage <|> parseSuppliedDAppMessage
  where parseEncodedDAppMessage = option (SuppliedDAppMessage <$> readEncodedDAppMessage) (long "message" <> short 'm' <> metavar "DAPPMSG" <> help "use an encoded DAppMessage")
        parseSuppliedDAppMessage = SuppliedDAppMessage <$> (parseLocationWithArbitraryDAppMessage <|> (Location <$> parseLocationDAppMessage)<|> (ArbitraryString <$> parseArbitraryDAppMessage))
        parseArbitraryDAppMessage = (strOption (long "arb" <> short 'a' <> help "make an arbitrary-string DAppMessage"))
        parseLocationDAppMessage = ado
          lat <- option number (long "lat" <> metavar "LAT" <> help "supply a latitude to the DAppMessage")
          lon <- option number (long "lon" <> metavar "LON" <> help "supply a longitude to the DAppMessage")
          in { lat, lon }
        parseLocationWithArbitraryDAppMessage = ado
          { lat, lon } <- parseLocationDAppMessage
          string <- parseArbitraryDAppMessage
          in LocationWithArbitrary { string, lat, lon }


newtype SignTransferOptions = SignTransferOptions
  { privateKey :: PrivateKey
  , nonce :: UIntN S32
  , feeAmount :: UIntN S128
  , tokenID :: UIntN S32
  , destination :: Address
  , transmitEndpoint :: Maybe String
  }

newtype SignMintOptions = SignMintOptions
  { privateKey :: PrivateKey
  , nonce :: UIntN S32
  , feeAmount :: UIntN S128
  , dataSource :: DataSource'
  , transmitEndpoint :: Maybe String
  }

newtype DecodeOptions =
  DecodeOptions DataSource

newtype ApproveFTOptions = ApproveFTOptions
  { privateKey :: PrivateKey
  , rnftAddress :: Maybe Address
  , ftAddress :: Maybe Address
  , nodeUrl :: Maybe String
  , chainID :: Maybe ChainId
  , fungibleTokenAmount :: Maybe (UIntN S256)
  , accountNonce :: Maybe BigNumber
  , gas :: Maybe BigNumber
  , gasPrice :: Maybe BigNumber
  , submitTxn :: Boolean
  }

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
    transmitEndpoint <- parseTransmitEndpoint
    in SignTransferOptions { privateKey, nonce, feeAmount, tokenID, destination, transmitEndpoint }


parseSignMintOptions :: Parser SignMintOptions
parseSignMintOptions = ado
    privateKey <- option readPrivateKey (long "private-key" <> short 'p' <> metavar "HEX" <> help "private key to sign with")
    nonce <- option (readUIntN s32) (long "nonce" <> short 'n' <> metavar "BIGNUM" <> help "nonce to use for message")
    feeAmount <- option (readUIntN s128) (long "fee-amount" <> short 'f' <> metavar "BIGNUM" <> help "fee amount to pay relayer")
    dataSource <- parseDataSource'
    transmitEndpoint <- parseTransmitEndpoint
    in SignMintOptions { privateKey, nonce, feeAmount, dataSource, transmitEndpoint }

parseDecodeOptions :: Parser DecodeOptions
parseDecodeOptions = DecodeOptions <$> parseDataSource "signed message"

parseApproveFTOptions :: Parser ApproveFTOptions
parseApproveFTOptions = ado
  privateKey <- option readPrivateKey (long "private-key" <> short 'p' <> metavar "HEX" <> help "private key to sign with")
  rnftAddress <- (Just <$> option readAddress (long "rnft" <> short 'r' <> metavar "ADDRESS" <> help "address of RelayableNFT contract (will try to read from chanterelle artifact + chain ID if missing)")) <|> pure Nothing
  ftAddress <- (Just <$> option readAddress (long "ft" <> short 'f' <> metavar "ADDRESS" <> help "address of FungibleToken contract  (will try to read from chanterelle artifact + chain ID if missing)")) <|> pure Nothing
  accountNonce <- (Just <$> option readBigNumber (long "nonce" <> short 'n' <> metavar "BIGNUM" <> help "Ethereum nonce to use for tx (will try to read from Ethereum if missing and --node-url is set")) <|> pure Nothing
  nodeUrl <- (Just <$> strOption (long "node-url" <> short 'u' <> metavar "URL" <> help "Ethereum Node URL to auto-fill missing values from")) <|> pure Nothing
  fungibleTokenAmount <- (Just <$> option (readUIntN s256) (long "amount" <> short 'a' <> metavar "BIGNUM" <> help "Amount of fungible token to approve (will use full balance if missing and --node-url is set")) <|> pure Nothing
  chainID <- (Just <$> option (ChainId <$> int) (long "chain-id" <> short 'c' <> metavar "INT" <> help "Chain ID that transaction will be sent (will try to read from Ethereum if missing and --node-url is set")) <|> pure Nothing
  submitTxn <- switch (long "submit" <> short 'X' <> help "Submit the transaction to the given node-url if its set")
  gas <- (Just <$> option readBigNumber (long "gas" <> short 'g' <> metavar "BIGNUM" <> help "gas limit to use (will use 1.25 txn estinmate from node if missing and --node-url is set")) <|> pure Nothing
  gasPrice <- (Just <$> option readBigNumber (long "gas-price" <> short 'G' <> metavar "BIGNUM" <> help "gas price to use (will use suggested price from node if missing and --node-url is set")) <|> pure Nothing
  in ApproveFTOptions { privateKey, rnftAddress, ftAddress, accountNonce, nodeUrl, fungibleTokenAmount, chainID, gas, gasPrice, submitTxn }


parseSubcommand :: Parser Subcommand
parseSubcommand = subparser 
                ( mkSubcommand "sign-transfer" "Sign a relayable transfer" (SignTransfer <$> parseSignTransferOptions)
               <> mkSubcommand "sign-mint" "Sign a relayable mint" (SignMint <$> parseSignMintOptions)
               <> mkSubcommand "decode" "Decode a relayable message" (Decode <$> parseDecodeOptions)
               <> mkSubcommand "generate-private-key" "Generate a private key to sign with" (pure GeneratePrivateKey)
               <> mkSubcommand "private-to-address" "Convert a private key to an Ethereum Address" (PrivateToAddress <$> (argument readPrivateKey $ metavar "HEX"))
               <> mkSubcommand "approve-fungible-token" "Generate an Ethereum transaction to approve RNFT to transfer FT" (ApproveFT <$> parseApproveFTOptions)
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

transmitMessageIfRequested :: Maybe String -> HexString -> Effect Unit
transmitMessageIfRequested mEP hex = launchAff_ do
  let rawBody = unHex hex
  liftEffect $ Console.log rawBody
  case mEP of
    Nothing -> pure unit
    Just ep -> do
      res <- AF.post ResponseFormat.string ep (Just $ RequestBody.string rawBody)
      case res of
        Left err -> throwError $ error $ AF.printError err
        Right ({ body, status }) ->
          if status == StatusCode 200
          then liftEffect $ Console.log $ "server replied: " <> body
          else throwError $ error $ "Got status " <> show status <> ": " <> body

runHelper :: Subcommand -> Effect Unit
runHelper (SignTransfer (SignTransferOptions { privateKey, nonce, feeAmount, tokenID, destination, transmitEndpoint })) = do
  let unsigned = UnsignedRelayedTransfer { nonce, feeAmount, tokenID, destination }
      signed = signRelayedTransfer privateKey unsigned
      packed = packSignedRelayedTransfer signed
      hex = fromByteString packed
  transmitMessageIfRequested transmitEndpoint hex
runHelper (SignMint (SignMintOptions { privateKey, nonce, feeAmount, dataSource, transmitEndpoint })) = do
  tokenData <- case dataSource of
    DataSource ds -> readDataSource ds
    SuppliedDAppMessage m -> pure (packDAppMessage m)
  let unsigned = UnsignedRelayedMessage { nonce, feeAmount, tokenData }
      signed = signRelayedMessage privateKey unsigned
      packed = packSignedRelayedMessage signed
      hex = fromByteString packed
  transmitMessageIfRequested transmitEndpoint hex
runHelper (Decode (DecodeOptions dataSource)) = do
  msg <- readDataSource dataSource
  let interpretedMsg = interpretDecodedMessage (hush <<< parseDAppMessage <<< fromUTF8) <$> decodePackedMessage msg
  Console.log <<< stringifyWithIndent 4 $ encodeJson interpretedMsg
runHelper GeneratePrivateKey = Console.log <<< show =<< generatePrivateKey
runHelper (PrivateToAddress private) = Console.log <<< show $ privateToAddress private
runHelper (ApproveFT (ApproveFTOptions args)) = do
  maybeProvider <- maybe (pure Nothing) (map Just <<< httpProvider) args.nodeUrl
  let failHard :: forall m a. MonadEffect m => Int -> String -> m a
      failHard exitCode reason = liftEffect $ log Error reason *> exit exitCode
  launchAff_ do
    let withProvider :: forall a. String -> (Web3Error -> Aff a) -> (a -> Aff Unit) -> Web3 a -> Aff a
        withProvider valueName onError successHook action = do
          case maybeProvider of
            Nothing -> failHard 1 $ "Couldn't get " <> valueName <> " as --node-url was unspecified"
            Just provider ->
              runWeb3 provider action >>= case _ of
                Left err -> onError err
                Right res -> successHook res *> pure res
        fillFromNode:: forall a. (a -> String) -> String -> Web3 a -> Aff a
        fillFromNode shower valueName = withProvider valueName (\err -> failHard 2 $ "Couldn't fill " <> valueName <> " due to Web3 error: " <> show err) (\a -> log Info ("Autofilled " <> valueName <> " from node as: " <> shower a))
    when (args.submitTxn && isNothing maybeProvider) $ failHard 1 "--submit-txn was set, but no --node-url was specified"
    let address = privateToAddress args.privateKey
    chainID <- maybe (fillFromNode show "--chain-id" ((map ChainId <<< Int.fromString) <$> net_version >>= (maybe (failHard 2 "Node returned an invalid Chain ID") pure))) pure args.chainID
    let fillFromArtifact v name path = do
          case v of
            Just v' -> pure v'
            Nothing -> (runExceptT $ readArtifact path) >>= case _ of
              Left err -> failHard 2 $ "Couldn't read artifact for " <> name <> " at " <> path <> ": " <> show err
              Right art ->
                let (ChainId chainID') = chainID
                    maddress = art ^? _network chainID' <<< _Just <<< _Deployed <<< _Just <<< _address
                 in case maddress of
                      Nothing -> failHard 2 $ "Couldn't find valid deploy address for chain ID " <> show chainID' <> " in artifact: " <> path
                      Just artAddress -> do
                        log Info $ "Autofilled " <> name <> " for " <> show chainID <> " from Chanterelle artifact at " <> path <> " as: " <> show artAddress
                        pure artAddress
    ftAddress <- fillFromArtifact args.ftAddress "--ft" "build/FungibleToken.json"
    rnftAddress <- fillFromArtifact args.rnftAddress "--ft" "build/RelayableNFT.json"
    let baseTxOpts = makeTxOpts { from: address, to: ftAddress }
    amount <- maybe (fillFromNode (show <<< unUIntN) "--amount" (either (throwError <<< error <<< show) pure =<< FT.balanceOf baseTxOpts Latest { account: address })) pure args.fungibleTokenAmount
    when (unUIntN amount == embed 0) $ failHard 3 $ "Account " <> show address <> " has a zero balance of FungibleToken at " <> show ftAddress <> ", so signing this approve() transaction is pointless"
    accountNonce <- fillFromNode show "--nonce" $ eth_getTransactionCount address Latest
    let txOpts = baseTxOpts # _nonce ?~ accountNonce # _gas .~ args.gas # _gasPrice .~ args.gasPrice
        approveArgs = { amount, spender: rnftAddress }
    txn <-
      if isNothing args.gas || isNothing args.gasPrice
      then withProvider "--gas and/or --gas-price" (\err -> failHard 3 $ "Couldn't fill --gas/--gas-price from node to sign the transaction due to a Web3 error: " <> show err) (\_ -> pure unit) (signApprovalTx' args.privateKey txOpts approveArgs)
      else pure $ signApprovalTx args.privateKey chainID txOpts approveArgs
    liftEffect $ Console.log $ show txn
    when args.submitTxn $
      withProvider "the transaction submitted as requested" (\err -> failHard 4 $ "Couldn't submit the transaction to the node due to a Web3 error: " <> show err) (\_ -> pure unit) $ do
        txh <- eth_sendRawTransaction txn
        provider <- ask
        txr <- pollTransactionReceipt txh provider
        pure unit