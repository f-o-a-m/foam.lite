module Main where

import Prelude

import Affjax as AF
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Chanterelle.Internal.Artifact (_Deployed, _address, _network, readArtifact)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Utils (getPrimaryAccount, pollTransactionReceipt)
import Contracts.FungibleToken as FT
import Contracts.RelayableNFT as RNFT
import Control.Alt ((<|>))
import Control.Error.Util (hush)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask)
import DApp.Message (DAppMessage(..), packDAppMessage, parseDAppMessage)
import DApp.Relay (UnsignedRelayedMessage(..), UnsignedRelayedTransfer(..), decodePackedMessage, interpretDecodedMessage, packSignedRelayedMessage, packSignedRelayedTransfer, signRelayedMessage, signRelayedTransfer)
import DApp.Util (makeTxOpts, signABIFn', signApprovalTx, signApprovalTx', signTransaction')
import Data.Argonaut (encodeJson)
import Data.Argonaut.Core (stringifyWithIndent)
import Data.Array as Array
import Data.ByteString (ByteString, fromUTF8, unsafeFreeze)
import Data.Either (Either(..), either, fromRight, note)
import Data.EitherR (fmapL)
import Data.Int as Int
import Data.Lens (_Just, (^?), (?~), (.~))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String as String
import Data.String.Regex as Regex
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error)
import Network.Ethereum.Core.BigNumber (BigNumber, decimal, embed, hexadecimal, parseBigNumber)
import Network.Ethereum.Core.HexString (HexString, fromByteString, mkHexString, toByteString, unHex)
import Network.Ethereum.Core.Signatures (Address, ChainId(..), PrivateKey, generatePrivateKey, mkAddress, mkPrivateKey, privateToAddress)
import Network.Ethereum.Web3 (class KnownSize, ChainCursor(..), DLProxy, TransactionReceipt(..), TransactionStatus(..), UIntN, Value, Web3, Web3Error, Wei, _gas, _gasPrice, _nonce, _value, formatValue, httpProvider, mkValue, runWeb3, sizeVal, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3.Api (eth_getBalance, eth_getTransactionCount, eth_sendRawTransaction, eth_sendTransaction, net_version)
import Network.Ethereum.Web3.Solidity.Sizes (S128, S256, S32, s128, s256, s32)
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Process (exit)
import Node.Process as NP
import Node.Stream as NS
import Options.Applicative (CommandFields, Mod, Parser, ReadM, argument, command, eitherReader, execParser, flag', fullDesc, header, help, helper, info, int, long, metavar, number, option, progDesc, readerError, short, showDefault, strOption, subparser, switch, value, (<**>))
import Partial.Unsafe (unsafePartialBecause)
import Type.Proxy (Proxy(..))

foreign import realConsoleLog :: String -> Effect Unit

data Subcommand = SignTransfer SignTransferOptions
                | SignMint SignMintOptions
                | Decode DecodeOptions
                | GeneratePrivateKey
                | PrivateToAddress PrivateKey
                | ApproveFT ApproveFTOptions
                | Faucet FaucetOptions
                | CheckBalances BalanceOptions
                | GetRelayNonce RelayNonceOptions

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

parseDataSource' :: Parser (Maybe DataSource')
parseDataSource' = ((Just <<< DataSource) <$> parseDataSource "minting") <|> parseSuppliedDAppMessage
  where parseEncodedDAppMessage = option (SuppliedDAppMessage <$> readEncodedDAppMessage) (long "message" <> short 'm' <> metavar "DAPPMSG" <> help "use an encoded DAppMessage")
        parseArbStr = (strOption (long "arb" <> short 'a' <> help "make an arbitrary-string DAppMessage"))
        parseLatLon = ado
          lat <- option number (long "lat" <> metavar "LAT" <> help "supply a latitude to the DAppMessage")
          lon <- option number (long "lon" <> metavar "LON" <> help "supply a longitude to the DAppMessage")
          in { lat, lon }
        parseSuppliedDAppMessage = ado
          ll <- (Just <$> parseLatLon) <|> pure Nothing
          str <- (Just <$> parseArbStr) <|> pure Nothing
          in SuppliedDAppMessage <$> case { ll, str } of
               { ll: Just latlon, str: Nothing } -> Just $ Location latlon
               { ll: Nothing, str: Just s } -> Just $ ArbitraryString s
               { ll: Just { lat, lon }, str: Just string } -> Just $ LocationWithArbitrary { lat, lon, string }
               _ -> Nothing

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
  , dataSource :: Maybe DataSource'
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

data AddressSource = ByPrivateKey PrivateKey | ByAddress Address | PrimaryAccount

newtype FaucetOptions = FaucetOptions
  { faucetFrom :: Maybe AddressSource
  , faucetTo :: AddressSource
  , nodeUrl :: String
  , ftAddress :: Maybe Address
  , fungibleTokenAmount :: UIntN S256
  , etherAmount :: UIntN S256
  , alsoEther :: Boolean
  }

newtype BalanceOptions = BalanceOptions
  { nodeUrl :: String
  , balanceOf :: AddressSource
  , ftAddress :: Maybe Address
  , rnftAddress :: Maybe Address
  }

newtype RelayNonceOptions = RelayNonceOptions
  { nodeUrl :: String
  , nonceOf :: AddressSource
  , rnftAddress :: Maybe Address
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
        commandDesc = (fullDesc <> progDesc desc <> header ("foam.lite-helper-" <> sc <> " - " <> desc))

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

parseNodeUrl :: Maybe String -> Parser String
parseNodeUrl def = strOption (long "node-url" <> short 'u' <> metavar "URL" <> help "Ethereum Node URL to auto-fill missing values from" <> maybeDefaults)
  where maybeDefaults = maybe mempty (\d -> value d <> showDefault) def

parseFungibleTokenAmount :: String -> Maybe (UIntN S256) -> Parser (UIntN S256)
parseFungibleTokenAmount action default = parseTokenAmount { action, tokenName: "FungibleToken", default, long: "amount", short: 'f' }

parseEtherAmount :: String -> Maybe (UIntN S256) -> Parser (UIntN S256)
parseEtherAmount action default = parseTokenAmount { action, tokenName: "Ether", default, long: "ether-amount", short: 'e' }

parseTokenAmount :: { action :: String, tokenName :: String, default :: Maybe (UIntN S256), long :: String, short :: Char } -> Parser (UIntN S256)
parseTokenAmount args = option (readUIntN s256) (long args.long <> short args.short <> metavar "BIGNUM" <> help ("Amount of " <> args.tokenName <> " to " <> args.action) <> def)
  where def = maybe mempty (\v -> value v <> showDefault) args.default

parseFTAddress :: Parser (Maybe Address)
parseFTAddress = (Just <$> option readAddress (long "ft" <> short 'f' <> metavar "ADDRESS" <> help "address of FungibleToken contract (will try to read from chanterelle artifact + chain ID if missing)")) <|> pure Nothing

parseRNFTAddress :: Parser (Maybe Address)
parseRNFTAddress = (Just <$> option readAddress (long "rnft" <> short 'r' <> metavar "ADDRESS" <> help "address of RelayableNFT contract (will try to read from chanterelle artifact + chain ID if missing)")) <|> pure Nothing

parseApproveFTOptions :: Parser ApproveFTOptions
parseApproveFTOptions = ado
  privateKey <- option readPrivateKey (long "private-key" <> short 'p' <> metavar "HEX" <> help "private key to sign with")
  rnftAddress <- parseRNFTAddress
  ftAddress <- parseFTAddress
  accountNonce <- (Just <$> option readBigNumber (long "nonce" <> short 'n' <> metavar "BIGNUM" <> help "Ethereum nonce to use for tx (will try to read from Ethereum if missing and --node-url is set")) <|> pure Nothing
  nodeUrl <- (Just <$> parseNodeUrl Nothing) <|> pure Nothing
  fungibleTokenAmount <- (Just <$> parseFungibleTokenAmount "approve" Nothing) <|> pure Nothing
  chainID <- (Just <$> option (ChainId <$> int) (long "chain-id" <> short 'c' <> metavar "INT" <> help "Chain ID that transaction will be sent (will try to read from Ethereum if missing and --node-url is set")) <|> pure Nothing
  submitTxn <- switch (long "submit" <> short 'X' <> help "Submit the transaction to the given node-url if its set")
  gas <- (Just <$> option readBigNumber (long "gas" <> short 'g' <> metavar "BIGNUM" <> help "gas limit to use (will use 1.25 txn estinmate from node if missing and --node-url is set")) <|> pure Nothing
  gasPrice <- (Just <$> option readBigNumber (long "gas-price" <> short 'G' <> metavar "BIGNUM" <> help "gas price to use (will use suggested price from node if missing and --node-url is set")) <|> pure Nothing
  in ApproveFTOptions { privateKey, rnftAddress, ftAddress, accountNonce, nodeUrl, fungibleTokenAmount, chainID, gas, gasPrice, submitTxn }

parseAddressSource :: { uppercaseShort :: Boolean, addressType :: Maybe String, addressDesc :: String, privateKeyDesc :: String } -> Parser AddressSource
parseAddressSource { uppercaseShort, addressType, addressDesc, privateKeyDesc } =
  let pfx = maybe "" (_ <> "-") addressType
      addressShort = if uppercaseShort then 'A' else 'a'
      privateKeyShort = if uppercaseShort then 'P' else 'p'
      parsePrivateKey = option (ByPrivateKey <$> readPrivateKey) (long (pfx <> "private-key") <> short privateKeyShort <> metavar "HEX" <> help ("private key to " <> privateKeyDesc))
      parseAddress = option (ByAddress <$> readAddress) (long (pfx <> "address") <> short addressShort <> metavar "HEX" <> help ("address to " <> addressDesc))
      parsePrimaryAccount = option (pure PrimaryAccount) (long (pfx <> "primary-account") <> help ("send " <> pfx <> " the primary account on the node"))
   in parsePrimaryAccount <|> parsePrivateKey <|> parseAddress

parseFaucetOptions :: Parser FaucetOptions
parseFaucetOptions = ado
  faucetFrom <- (Just <$> parseAddressSource { uppercaseShort: true, addressType: Just "from", privateKeyDesc: "sign faucet txn with", addressDesc: "send tokens from (must be unlocked on node)" }) <|> pure Nothing
  faucetTo <- parseAddressSource { uppercaseShort: false, addressType: Just "to", privateKeyDesc: "generate destination address from", addressDesc: "send tokens to" }
  nodeUrl <- parseNodeUrl (Just "http://localhost:8545")
  fungibleTokenAmount <- parseFungibleTokenAmount "send" (uIntNFromBigNumber s256 =<< parseBigNumber decimal "100000000000000000000")
  etherAmount <- parseEtherAmount "send if --also-ether/-E is set" (uIntNFromBigNumber s256 =<< parseBigNumber decimal "2500000000000000000")
  alsoEther <- switch (long "also-ether" <> short 'E' <> help "Also send a small amount of ether to cover the approve() tx")
  ftAddress <- parseFTAddress
  in FaucetOptions { faucetFrom, faucetTo, nodeUrl, fungibleTokenAmount, ftAddress, etherAmount, alsoEther }

parseRelayNonceOptions :: Parser RelayNonceOptions
parseRelayNonceOptions = ado
  nodeUrl <- parseNodeUrl (Just "http://localhost:8545")
  nonceOf <- parseAddressSource { uppercaseShort: false, addressType: Nothing, privateKeyDesc: "generate address to check balances of", addressDesc: "check balances of"}
  rnftAddress <- parseRNFTAddress
  in RelayNonceOptions { nodeUrl, nonceOf, rnftAddress }

parseBalanceOptions :: Parser BalanceOptions
parseBalanceOptions = ado
  RelayNonceOptions { nodeUrl, nonceOf: balanceOf, rnftAddress } <- parseRelayNonceOptions
  ftAddress <- parseFTAddress
  in BalanceOptions { nodeUrl, balanceOf, ftAddress, rnftAddress }
  

parseSubcommand :: Parser Subcommand
parseSubcommand = subparser 
                ( mkSubcommand "sign-transfer" "Sign a relayable transfer" (SignTransfer <$> parseSignTransferOptions)
               <> mkSubcommand "sign-mint" "Sign a relayable mint" (SignMint <$> parseSignMintOptions)
               <> mkSubcommand "decode" "Decode a relayable message" (Decode <$> parseDecodeOptions)
               <> mkSubcommand "generate-private-key" "Generate a private key to sign with" (pure GeneratePrivateKey)
               <> mkSubcommand "private-to-address" "Convert a private key to an Ethereum Address" (PrivateToAddress <$> (argument readPrivateKey $ metavar "HEX"))
               <> mkSubcommand "approve" "Generate an Ethereum transaction to approve RNFT to transfer FT" (ApproveFT <$> parseApproveFTOptions)
               <> mkSubcommand "faucet" "Send FT to an account" (Faucet <$> parseFaucetOptions)
               <> mkSubcommand "balance" "Check token balances of an account (alias for `balances`)" (CheckBalances <$> parseBalanceOptions)
               <> mkSubcommand "balances" "Check token balances of an account" (CheckBalances <$> parseBalanceOptions)
               <> mkSubcommand "nonce" "Check the expected relay nonce of account" (GetRelayNonce <$> parseRelayNonceOptions)
                )

main :: Effect Unit
main = runHelper =<< execParser opts
  where opts = info (parseSubcommand <**> helper)
          (  fullDesc
          <> progDesc "Helpers for the FOAM.lite App"
          <> header "foam.lite-helper - a reference for the rest of us" 
          )

readDataSource :: DataSource -> Effect ByteString
readDataSource Stdin = maybe (throwError $ error "Couldn't read anything from `stdin`")  (pure <<< unsafeFreeze) =<< NS.read NP.stdin Nothing
readDataSource (File fp) = unsafeFreeze <$> FS.readFile fp
readDataSource (Raw bs) = pure bs

transmitMessageIfRequested :: Maybe String -> HexString -> Effect Unit
transmitMessageIfRequested mEP hex = launchAff_ do
  let rawBody = unHex hex
  liftEffect $ realConsoleLog rawBody
  case mEP of
    Nothing -> pure unit
    Just ep -> do
      res <- AF.post ResponseFormat.string ep (Just $ RequestBody.string rawBody)
      case res of
        Left err -> throwError $ error $ AF.printError err
        Right ({ body, status }) ->
          if status == StatusCode 200
          then liftEffect $ realConsoleLog $ "server replied: " <> body
          else throwError $ error $ "Got status " <> show status <> ": " <> body

failHard :: forall m a. MonadEffect m => Int -> String -> m a
failHard exitCode reason = liftEffect $ log Error reason *> exit exitCode

fillFromArtifact :: Maybe Address -> ChainId -> String -> FilePath -> Aff Address
fillFromArtifact v chainID name path = do
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

runHelper :: Subcommand -> Effect Unit
runHelper (SignTransfer (SignTransferOptions { privateKey, nonce, feeAmount, tokenID, destination, transmitEndpoint })) = do
  let unsigned = UnsignedRelayedTransfer { nonce, feeAmount, tokenID, destination }
      signed = signRelayedTransfer privateKey unsigned
      packed = packSignedRelayedTransfer signed
      hex = fromByteString packed
  transmitMessageIfRequested transmitEndpoint hex
runHelper (SignMint (SignMintOptions { privateKey, nonce, feeAmount, dataSource, transmitEndpoint })) = do
  tokenData <- case dataSource of
    Just (DataSource ds) -> readDataSource ds
    Just (SuppliedDAppMessage m) -> pure (packDAppMessage m)
    Nothing -> throwError $ error "Missing DApp Message data: try --lat and --lon, --arb (or all three) to generate a message from args, or supply one directly with--stdin, --file, or --hex"
  let unsigned = UnsignedRelayedMessage { nonce, feeAmount, tokenData }
      signed = signRelayedMessage privateKey unsigned
      packed = packSignedRelayedMessage signed
      hex = fromByteString packed
  transmitMessageIfRequested transmitEndpoint hex
runHelper (Decode (DecodeOptions dataSource)) = do
  msg <- readDataSource dataSource
  let interpretedMsg = interpretDecodedMessage (hush <<< parseDAppMessage <<< fromUTF8) <$> decodePackedMessage msg
  realConsoleLog <<< stringifyWithIndent 4 $ encodeJson interpretedMsg
runHelper GeneratePrivateKey = realConsoleLog <<< show =<< generatePrivateKey
runHelper (PrivateToAddress private) = realConsoleLog <<< show $ privateToAddress private
runHelper (ApproveFT (ApproveFTOptions args)) = do
  maybeProvider <- maybe (pure Nothing) (map Just <<< httpProvider) args.nodeUrl
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
    ftAddress <- fillFromArtifact args.ftAddress chainID "--ft" "build/FungibleToken.json"
    rnftAddress <- fillFromArtifact args.rnftAddress chainID "--rnft" "build/RelayableNFT.json"
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
    liftEffect $ realConsoleLog $ show txn
    when args.submitTxn $
      withProvider "the transaction submitted as requested" (\err -> failHard 4 $ "Couldn't submit the transaction to the node due to a Web3 error: " <> show err) (\_ -> pure unit) $ do
        txh <- eth_sendRawTransaction txn
        provider <- ask
        txr <- pollTransactionReceipt txh provider
        pure unit
runHelper (Faucet (FaucetOptions f)) = do
  provider <- httpProvider f.nodeUrl
  launchAff_ do
    let runWeb3' :: Web3 ~> Aff
        runWeb3' a = either (throwError <<< error <<< show) pure =<< (runWeb3 provider a)
        getPrimaryAccount' = runWeb3' getPrimaryAccount
        resolveAddressSource m = case m of
          PrimaryAccount -> runWeb3' getPrimaryAccount
          ByAddress a -> pure a
          ByPrivateKey p -> pure $ privateToAddress p
        pollTxr' tokenName txh = do 
          (TransactionReceipt txr) <- pollTransactionReceipt txh provider
          case txr.status of
            Succeeded -> log Info $ tokenName <> " transfer succeeded"
            Failed -> log Error $ tokenName <> " transfer failed"
    sourceAddress <- maybe getPrimaryAccount' resolveAddressSource f.faucetFrom
    destinationAddress <- resolveAddressSource f.faucetTo
    chainID <- runWeb3' $ ((map ChainId <<< Int.fromString) <$> net_version) >>= (maybe (failHard 2 "Node returned an invalid Chain ID") pure)
    ftAddress <- fillFromArtifact f.ftAddress chainID "--ft" "build/FungibleToken.json"
    let tokenTxOpts = makeTxOpts { from: sourceAddress, to: ftAddress }
        tokenTxArgs = { amount: f.fungibleTokenAmount, recipient: destinationAddress }
    runWeb3' $ case f.faucetFrom of
      Just (ByPrivateKey p) -> signABIFn' (Proxy :: Proxy FT.TransferFn) p tokenTxOpts tokenTxArgs >>= eth_sendRawTransaction >>= pollTxr' "FungibleToken"
      _ -> FT.transfer tokenTxOpts tokenTxArgs >>= pollTxr' "FungibleToken"
    when f.alsoEther $ do
      let ethTxOpts = makeTxOpts { from: sourceAddress, to: destinationAddress } # _value ?~ (mkValue <<< unUIntN $ f.etherAmount :: Value Wei)
      runWeb3' $ case f.faucetFrom of
          Just (ByPrivateKey p) -> signTransaction' p ethTxOpts >>= eth_sendRawTransaction >>= pollTxr' "ETH"
          _ -> eth_sendTransaction ethTxOpts >>= pollTxr' "ETH"
    pure unit
runHelper (CheckBalances (BalanceOptions f)) = do
  provider <- httpProvider f.nodeUrl
  launchAff_ do
    let runWeb3' :: Web3 ~> Aff
        runWeb3' a = either (throwError <<< error <<< show) pure =<< (runWeb3 provider a)
        resolveAddressSource m = case m of
          PrimaryAccount -> runWeb3' getPrimaryAccount
          ByAddress a -> pure a
          ByPrivateKey p -> pure $ privateToAddress p
    chainID <- runWeb3' $ ((map ChainId <<< Int.fromString) <$> net_version) >>= (maybe (failHard 2 "Node returned an invalid Chain ID") pure)
    ftAddress <- fillFromArtifact f.ftAddress chainID "--ft" "build/FungibleToken.json"
    rnftAddress <- fillFromArtifact f.rnftAddress chainID "--rnft" "build/RelayableNFT.json"
    address <- resolveAddressSource f.balanceOf
    let tokenTxOpts = makeTxOpts { from: address, to: ftAddress }
    ethBal <- formatBalance <$> (runWeb3' $ eth_getBalance address Latest)
    tokenBal <- formatBalance <$> (runWeb3' $ either (throwError <<< error <<< show) (pure <<< unUIntN) =<< FT.balanceOf tokenTxOpts Latest { account: address})
    approvedBal <- formatBalance <$> (runWeb3' $ either (throwError <<< error <<< show) (pure <<< unUIntN) =<< FT.allowance tokenTxOpts Latest { owner: address, spender: rnftAddress })
    let biggestLength = max (String.length ethBal) (String.length tokenBal)
    log Info $ "ETH balance:    " <> (leftPad ' ' biggestLength ethBal)
    log Info $ "FT balance:     " <> (leftPad ' ' biggestLength tokenBal)
    log Info $ "RNFT allowance: " <> (leftPad ' ' biggestLength approvedBal)
runHelper (GetRelayNonce (RelayNonceOptions f)) = do
  provider <- httpProvider f.nodeUrl
  launchAff_ do
    let runWeb3' :: Web3 ~> Aff
        runWeb3' a = either (throwError <<< error <<< show) pure =<< (runWeb3 provider a)
        resolveAddressSource m = case m of
          PrimaryAccount -> runWeb3' getPrimaryAccount
          ByAddress a -> pure a
          ByPrivateKey p -> pure $ privateToAddress p
    chainID <- runWeb3' $ ((map ChainId <<< Int.fromString) <$> net_version) >>= (maybe (failHard 2 "Node returned an invalid Chain ID") pure)
    rnftAddress <- fillFromArtifact f.rnftAddress chainID "--rnft" "build/RelayableNFT.json"
    addr <- resolveAddressSource f.nonceOf
    let txOpts = makeTxOpts { from: addr, to: rnftAddress }
    nonce <- runWeb3' $ (either (throwError <<< error <<< show) (pure <<< unUIntN) =<< RNFT.getCurrentRelayNonce txOpts Latest { addr })
    liftEffect <<< realConsoleLog $ show nonce

leftPad :: Char -> Int -> String -> String
leftPad padChar desiredLen str = 
  let currentLen = String.length str
      neededChars = desiredLen - currentLen
      extraChars = if neededChars > 0 then String.fromCodePointArray (Array.replicate (desiredLen - currentLen) $ String.codePointFromChar padChar) else ""
   in extraChars <> str

formatBalance :: BigNumber -> String
formatBalance num =
  let numToString = formatValue (mkValue num :: Value Wei)
      reverse = String.fromCodePointArray <<< Array.reverse <<< String.toCodePointArray
      { before, after } = String.splitAt 18 $ reverse numToString
   in if String.null after
      then "0." <> leftPad '0' 18 (reverse before)
      else (reverse after) <> "." <> (reverse before)
