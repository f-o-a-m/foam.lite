module Routes.Relay where

import Prelude

import Contracts.RelayableNFT as RNFT
import Control.Monad.Except (ExceptT, except, throwError, withExceptT)
import Control.Monad.Reader (ask)
import DApp.Message (DAppMessage, parseDAppMessage)
import DApp.Relay (SignedRelayedMessage, SignedRelayedTransfer(..), recoverRelayedTransferSignerWeb3)
import DApp.Relay.Types (DecodedMessage(..), InterpretedDecodedMessage, decodePackedMessage, interpretDecodedMessage)
import DApp.Util (makeTxOpts, widenUIntN32)
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.ByteString as BS
import Data.Either (Either(..), either, hush)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (liftAff)
import MIME (class FromByteString, class FromString, OctetStream, PlainText, TroutWrapper(..), fromString, toByteString)
import Network.Ethereum.Core.HexString (HexString, fromByteString)
import Network.Ethereum.Core.Signatures (Address, nullAddress)
import Network.Ethereum.Web3 (BigNumber, ChainCursor(..), Transaction(..), TransactionReceipt(..), TransactionStatus(..), Web3Error(..), runWeb3)
import Network.Ethereum.Web3.Api (eth_getTransaction, eth_getTransactionReceipt)
import Network.Ethereum.Web3.Types (RpcError(..))
import Nodetrout (HTTPError, error400, error500)
import Type.Quotient (mkQuotient)
import Type.Trout (type (:/), type (:<|>), type (:=), type (:>), Capture, ReqBody, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Post, Get)
import Types (AppM)

newtype RelayRequestBody = RelayRequestBody BS.ByteString

instance fromByteStringRelayRequestBody :: FromByteString RelayRequestBody where
  fromByteString = pure <<< RelayRequestBody

instance decodeJsonRelayRequestBody :: DecodeJson RelayRequestBody where
  decodeJson o = do
    x :: HexString <- decodeJson o
    pure $ RelayRequestBody $ toByteString x

instance fromStringRelayRequestBody :: FromString RelayRequestBody where
  fromString = map (RelayRequestBody <<< toByteString) <<< (fromString :: String -> Either String HexString)

type RelayRoute = "relay" := ("relay" :/ RelaySubroutes)
type RelaySubroutes =     ("submit" := "submit" :/ ReqBody RelayRequestBody (PlainText :<|> OctetStream) :> Resource (Post HexString (PlainText :<|> OctetStream)))
                    :<|>  ("validate" := "validate" :/ ReqBody RelayRequestBody ((PlainText :<|> OctetStream)) :> Resource (Post (InterpretedDecodedMessage DAppMessage) JSON))
                    :<|>  ("nonce" := "nonce" :/ Capture "address" (TroutWrapper Address) :> Resource (Get BigNumber (PlainText :<|> OctetStream)))
                    :<|>  ("tx_status" := "status" :/ Capture "txhash" (TroutWrapper HexString) :> Resource (Get HexString PlainText))

data TxStatus = Mined TransactionStatus | Mempool | Nonexistent | Invalid

txStatusToHexString :: TxStatus -> HexString
txStatusToHexString s =
  let octet = case s of
                Mined Failed -> 0
                Mined Succeeded -> 1
                Mempool -> 2
                Nonexistent -> 3
                Invalid -> 4
   in fromByteString $ BS.singleton (mkQuotient octet)

relayRoute :: _
relayRoute = { 
  "submit": postSubmitRoute,
  "validate": postValidateRoute,
  "nonce": getNonceRoute,
  "tx_status": getTxStatusRoute
}

-- | If we ever run into the *very* unlikely chance that a message can be decoded as both a transfer and a mint
-- | We check if the *transfer* could potentially be executed (i.e., the recovered signer actually owns the
-- | tokenID requested to be transferred). That way, we know the transfer for sure will fail, and break the
-- | tie with the potential of the mint succeeding...
validateTransfer :: SignedRelayedTransfer -> ExceptT HTTPError AppM Boolean
validateTransfer srt@(SignedRelayedTransfer t) = do
  { addresses, provider } <- ask
  let txOpts = makeTxOpts { from: nullAddress, to: addresses.relayableNFT }
  res <- liftAff $ runWeb3 provider do
    owner <- RNFT.ownerOf txOpts Latest { tokenId: widenUIntN32 t.tokenID }
    signer <- recoverRelayedTransferSignerWeb3 srt txOpts Latest
    pure { owner, signer }
  case res of
    Left err -> throwError error500
    Right res' -> pure $ res'.owner == res'.signer

resolveRelayRequestBody :: Maybe DecodedMessage -> ExceptT HTTPError AppM (Either SignedRelayedMessage SignedRelayedTransfer)
resolveRelayRequestBody Nothing = throwError error400
resolveRelayRequestBody (Just (DecodedMint m)) = pure $ Left m
resolveRelayRequestBody (Just (DecodedTransfer t)) = pure $ Right t
resolveRelayRequestBody (Just (DecodedBoth { mint, transfer })) = do
  isValidTransfer <- validateTransfer transfer
  pure $ 
    if isValidTransfer
    then Right transfer
    else Left mint

performRelayAction :: Either SignedRelayedMessage SignedRelayedTransfer -> ExceptT HTTPError AppM HexString
performRelayAction body = do
  { provider, relayActions } <- ask
  let action = either relayActions.doMintRelayed relayActions.doTransferRelayed body
  withExceptT web3ErrorToHTTPError (except =<< liftAff (runWeb3 provider action))

postSubmitRoute :: RelayRequestBody -> { "POST" :: ExceptT HTTPError AppM HexString }
postSubmitRoute (RelayRequestBody b) = {
    "POST": resolveRelayRequestBody (decodePackedMessage b) >>= performRelayAction
}

web3ErrorToHTTPError :: Web3Error -> HTTPError
web3ErrorToHTTPError e = 
  let baseError = error500
      showWeb3Error = show e
      expandedDetails = maybe showWeb3Error (_ <> ": " <> showWeb3Error) baseError.details
   in baseError { details = Just expandedDetails }

postValidateRoute :: RelayRequestBody -> { "POST" :: ExceptT HTTPError AppM (InterpretedDecodedMessage DAppMessage) }
postValidateRoute (RelayRequestBody b) = {
  "POST": maybe (throwError error400) pure (interpretDecodedMessage (hush <<< parseDAppMessage <<< BS.fromUTF8) <$> decodePackedMessage b)
}

getNonceRoute :: TroutWrapper Address -> { "GET" :: ExceptT HTTPError AppM BigNumber }
getNonceRoute (TroutWrapper address) = {
  "GET": ask >>= \{ provider, relayActions } -> withExceptT web3ErrorToHTTPError (except =<< liftAff (runWeb3 provider $ relayActions.getRelayNonce address))
}

getTxStatusRoute :: TroutWrapper HexString -> { "GET" :: ExceptT HTTPError AppM HexString }
getTxStatusRoute (TroutWrapper txHash) = { "GET": ask >>= (\{ provider } -> txStatusToHexString <$> go provider false) }
  where
    go provider isRetry = do
      eTxr <- liftAff $ runWeb3 provider $ eth_getTransactionReceipt txHash
      case eTxr of
        Left (Rpc (RpcError e)) ->
          if e.code == -32602
          then pure Invalid
          else checkForMempool provider isRetry
        Left e@NullError ->
          if isRetry
          then throwError $ web3ErrorToHTTPError e
          else checkForMempool provider true
        Left x -> throwError (web3ErrorToHTTPError x)
        Right (TransactionReceipt txr) -> pure $ Mined txr.status

    checkForMempool provider isRetry = do
      eTx <- liftAff $ runWeb3 provider $ eth_getTransaction txHash
      case eTx of
        Left NullError -> pure Nonexistent
        Left e -> throwError $ web3ErrorToHTTPError e
        Right (Transaction tx) -> case tx.blockNumber of
          Nothing -> pure Mempool
          Just blockNumber -> go provider isRetry -- Check again for a receipt, as txn mightve gotten mined in between the two calls