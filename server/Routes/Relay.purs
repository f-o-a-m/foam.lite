module Routes.Relay where

import Prelude

import Contracts.RelayableNFT as RNFT
import Control.Monad.Except (ExceptT, except, throwError, withExceptT)
import Control.Monad.Reader (ask)
import DApp.Message (DAppMessage, parseDAppMessage)
import DApp.Relay (SignedRelayedMessage, SignedRelayedTransfer(..), mintRelayed, recoverRelayedTransferSignerWeb3, transferRelayed)
import DApp.Relay.Types (DecodedMessage(..), InterpretedDecodedMessage, decodePackedMessage, interpretDecodedMessage)
import DApp.Util (makeTxOpts, widenUIntN32)
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.ByteString as BS
import Data.Either (Either(..), either, hush)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (liftAff)
import MIME (class FromString, PlainText, fromString, toByteString)
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Core.Signatures (nullAddress)
import Network.Ethereum.Web3 (ChainCursor(..), TransactionOptions, Web3, Web3Error, runWeb3)
import Network.Ethereum.Web3.Types (NoPay)
import Nodetrout (HTTPError, error400, error500)
import Type.Trout (type (:/), type (:=), type (:>), type (:<|>), ReqBody, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Post)
import Types (AppM)

newtype RelayRequestBody = RelayRequestBody BS.ByteString

instance decodeJsonRelayRequestBody :: DecodeJson RelayRequestBody where
  decodeJson o = do
    x :: HexString <- decodeJson o
    pure $ RelayRequestBody $ toByteString x

instance fromStringRelayRequestBody :: FromString RelayRequestBody where
  fromString = map (RelayRequestBody <<< toByteString) <<< (fromString :: String -> Either String HexString)

type SupportedResultMimes = (JSON :<|> PlainText)
type RelayRoute = "relay" := ("relay" :/ RelaySubroutes)
type RelaySubroutes =     ("submit" := "submit" :/ ReqBody RelayRequestBody PlainText :> Resource (Post HexString SupportedResultMimes))
                    :<|>  ("validate" := "validate" :/ ReqBody RelayRequestBody PlainText :> Resource (Post (InterpretedDecodedMessage DAppMessage) SupportedResultMimes))

relayRoute :: _
relayRoute = { 
  "submit": postSubmitRoute,
  "validate": postValidateRoute
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

bodyToAction :: Either SignedRelayedMessage SignedRelayedTransfer -> TransactionOptions NoPay -> Web3 HexString
bodyToAction = either mintRelayed transferRelayed

postSubmitRoute :: RelayRequestBody -> { "POST" :: ExceptT HTTPError AppM HexString }
postSubmitRoute (RelayRequestBody b) = {
    "POST": do
      resolved <- resolveRelayRequestBody (decodePackedMessage b)
      { addresses, provider } <- ask
      let txOpts = makeTxOpts { from: addresses.primaryAccount, to: addresses.relayableNFT }
      withExceptT web3ErrorToHTTPError (except =<< liftAff (runWeb3 provider (bodyToAction resolved txOpts)))
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

