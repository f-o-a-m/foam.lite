module Lora.FoamBridge (
    performPUSH_DATAAction
) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import DApp.Support (AppEnv)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import DApp.Relay.Types (DecodedMessage(..), decodePackedMessage)
import Data.ByteString (ByteString, fromString, toString, Encoding(..), tail)
import Network.Ethereum.Web3 (Web3Error(NullError), runWeb3)
import Effect.Aff (Aff)
import Network.Ethereum.Core.HexString (HexString, nullWord)
import Lora.UDP.PushDataJSON (PushDataJSON(..), ReceivedPacket(..), Base64Encoded(..))

performPUSH_DATAAction :: AppEnv -> PushDataJSON -> Aff Unit
performPUSH_DATAAction env (PushDataJSON { rxpk: Just [ (ReceivedPacket{ data: b64enc }) ]}) = do
    let bs = getPackedMessage b64enc
    let hbs = (\bs' -> toString bs' Hex) <$> bs
    let msg = decodePackedMessage' bs
    log Info $ show hbs
    log Info $ show bs
    log Info $ show msg
    result <- case msg of
                        Just m -> do
                            log Info $ "relaying message " <> (show m)
                            testAndRunRelay env m
                        _ -> pure $ Left NullError
    case result of
        Left NullError -> log Warn "PUSH DATA packet payload cannot be decoded as a FOAM message"
        Left err -> log Error $ show err
        Right h -> log Info $ "transaction succesfully relayed with id " <> show h
performPUSH_DATAAction _ _ = log Info "Empty Push Data packet"

getPackedMessage :: Base64Encoded -> Maybe ByteString
getPackedMessage (Base64Encoded b64enc) = fromString b64enc Base64 >>= tail

decodePackedMessage' :: Maybe ByteString -> Maybe DecodedMessage
decodePackedMessage' mbs = mbs >>= decodePackedMessage

testAndRunRelay :: AppEnv -> DecodedMessage -> Aff (Either Web3Error HexString)
testAndRunRelay ({ provider, relayActions }) msg = case msg of
    DecodedMint m -> do
        log Debug $ "doing dry run of " <> (show m)
        drr <- runWeb3 provider $ relayActions.callMintRelayed m
        log Debug $ "dry run passed passed: " <> (show drr)
        log Debug $ "sumitting to blockchain" <> (show m)
        runWeb3 provider $ relayActions.doMintRelayed m
    DecodedTransfer t -> do
        log Debug $ "doing dry run of " <> (show t)
        drr <- runWeb3 provider $ relayActions.callTransferRelayed t
        log Debug $ "dry run passed passed: " <> (show drr)
        log Debug $ "sumitting to blockchain" <> (show t)
        runWeb3 provider $ relayActions.doTransferRelayed t
    _ -> do
        log Error "unrecognized message type"
        pure $ Left NullError

testRelay :: AppEnv -> DecodedMessage -> Aff (Either Web3Error HexString)
testRelay ({ provider, relayActions }) msg = case msg of
    DecodedMint m -> runWeb3 provider $ relayActions.callMintRelayed m
    DecodedTransfer t -> runWeb3 provider $ relayActions.callTransferRelayed t
    _ -> pure $ Left NullError

runRelay :: AppEnv -> DecodedMessage -> Aff (Either Web3Error HexString)
runRelay ({ provider, relayActions }) msg = case msg of
    DecodedMint m -> runWeb3 provider $ relayActions.doMintRelayed m
    DecodedTransfer t -> runWeb3 provider $ relayActions.doTransferRelayed t
    _ -> pure $ Left NullError
