module Lora.FoamBridge (
    performPUSH_DATAAction
) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either, hush)
import Effect.Exception (message)
import Effect (Effect)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import DApp.Relay (SignedRelayedMessage, SignedRelayedTransfer(..), recoverRelayedTransferSignerWeb3)
import DApp.Relay.Types (DecodedMessage(..), InterpretedDecodedMessage, decodePackedMessage, interpretDecodedMessage)
import Data.ByteString (ByteString, fromString, toString, Encoding(..), tail)
import Data.ArrayView (toArray)
import Data.String (drop)
import Types (AppEnv)
import Network.Ethereum.Web3 (BigNumber, ChainCursor(..), Transaction(..), TransactionReceipt(..), TransactionStatus(..), Web3Error(..), runWeb3)
import Effect.Aff (Aff)
import Network.Ethereum.Core.HexString (HexString(..), nullWord)


import Lora.UDP.Pkt


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
                            log Info "relaying message"
                            runRelay env m
                        _ -> pure $ Right nullWord
    case result of
        Left err -> log Error $ show err
        Right h -> log Info $ show h

performPUSH_DATAAction _ _ = log Info "Empty Push Data packet"


getPackedMessage :: Base64Encoded -> Maybe ByteString
getPackedMessage (Base64Encoded b64enc) = do
    bs <- fromString b64enc Base64
    tail bs

decodePackedMessage' :: Maybe ByteString -> Maybe DecodedMessage
decodePackedMessage' mbs = do
    bs <- mbs
    decodePackedMessage bs

runRelay :: AppEnv -> DecodedMessage -> Aff (Either Web3Error HexString)
runRelay ({ provider, relayActions }) msg = case msg of
         DecodedMint m -> runWeb3 provider $ relayActions.doMintRelayed m
         DecodedTransfer t -> runWeb3 provider $ relayActions.doTransferRelayed t
         _ -> pure $ Right $ nullWord
