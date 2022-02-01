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
import Network.Ethereum.Core.HexString (HexString)
import Lora.UDP.PushDataJSON (PushDataJSON(..), ReceivedPacket(..), Base64Encoded(..))
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), throwError)
import Control.Monad.Trans.Class (lift)
import Control.Bind (bindFlipped)

performPUSH_DATAAction :: AppEnv -> PushDataJSON -> Aff Unit
performPUSH_DATAAction env (PushDataJSON { rxpk: Just [ (ReceivedPacket{ data: b64enc }) ]}) = do
    let bs = getPackedMessage b64enc
        hbs = (\bs' -> toString bs' Hex) <$> bs
        msg = decodePackedMessage' bs
    result <- case msg of
        Just m -> do
            log Info $ "relaying message " <> (show m)
            testAndRunRelay env m
        _ -> pure $ Left NullError
    case result of
        Left NullError -> log Warn "PUSH DATA packet payload cannot be decoded as a FOAM message"
        Left err -> log Error $ "error when doing relay: " <> show err
        Right h -> log Info $ "transaction succesfully relayed with id " <> show h
performPUSH_DATAAction _ _ = log Info "Empty Push Data packet"

getPackedMessage :: Base64Encoded -> Maybe ByteString
getPackedMessage (Base64Encoded b64enc) = fromString b64enc Base64 >>= tail

decodePackedMessage' :: Maybe ByteString -> Maybe DecodedMessage
decodePackedMessage' = bindFlipped decodePackedMessage

testAndRunRelay :: AppEnv -> DecodedMessage -> Aff (Either Web3Error HexString)
testAndRunRelay env msg = runExceptT do
    testRelay env msg
    runRelay env msg

testRelay :: AppEnv -> DecodedMessage -> ExceptT Web3Error Aff Unit
testRelay ({ provider, relayActions }) msg = do
    log Debug $ "doing dry run of " <> (show msg)
    drr <- case msg of
        DecodedMint m -> lift $ runWeb3 provider $ relayActions.callMintRelayed m
        DecodedTransfer t -> lift $ runWeb3 provider $ relayActions.callTransferRelayed t
        _ -> throwError NullError
    log Debug $ "dry run result: " <> (show drr)
    void $ ExceptT $ pure $ drr

runRelay :: AppEnv -> DecodedMessage -> ExceptT Web3Error Aff HexString
runRelay ({ provider, relayActions }) msg = do
    log Debug $ "sumitting to blockchain" <> (show msg)
    case msg of
        DecodedMint m -> ExceptT $ runWeb3 provider $ relayActions.doMintRelayed m
        DecodedTransfer t -> ExceptT $ runWeb3 provider $ relayActions.doTransferRelayed t
        _ -> throwError NullError
