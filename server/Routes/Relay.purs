module Routes.Relay where

import Prelude

import Contracts.RelayableNFT as RNFT
import Control.Monad.Except (ExceptT, except, throwError, withExceptT)
import Control.Monad.Reader (ask)
import DApp.Relay (SignedRelayedMessage, SignedRelayedTransfer(..), mintRelayed, parseSignedRelayedMessage, parseSignedRelayedTransfer, recoverRelayedTransferSignerWeb3, transferRelayed)
import DApp.Util (makeTxOpts, widenUIntN32)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.ByteString as BS
import Data.Either (Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
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

data DecodedBody = DecodedMint SignedRelayedMessage | DecodedTransfer SignedRelayedTransfer

instance encodeJsonDecodedBody :: EncodeJson DecodedBody where
  encodeJson = genericEncodeJson

derive instance genericDecodedBody :: Generic DecodedBody _
instance showDecodedBody :: Show DecodedBody where
  show = genericShow

instance decodeJsonRelayRequestBody :: DecodeJson RelayRequestBody where
  decodeJson o = do
    x :: HexString <- decodeJson o
    pure $ RelayRequestBody $ toByteString x

instance fromStringRelayRequestBody :: FromString RelayRequestBody where
  fromString = map (RelayRequestBody <<< toByteString) <<< (fromString :: String -> Either String HexString)

type SupportedResultMimes = (JSON :<|> PlainText)
type RelayRoute = "relay" := ("relay" :/ RelaySubroutes)
type RelaySubroutes =     ("submit" := "submit" :/ ReqBody RelayRequestBody PlainText :> Resource (Post HexString SupportedResultMimes))
                    :<|>  ("validate" := "validate" :/ ReqBody RelayRequestBody PlainText :> Resource (Post DecodedBody SupportedResultMimes))


relayRoute :: _
relayRoute = { 
  "submit": postSubmitRoute,
  "validate": postValidateRoute
}

type DecodableAsBoth = { mint :: Maybe SignedRelayedMessage, transfer :: Maybe SignedRelayedTransfer }

decodeRelayRequestBody :: RelayRequestBody -> DecodableAsBoth
decodeRelayRequestBody (RelayRequestBody b) = 
  let mint = hush $ parseSignedRelayedMessage b
      transfer = hush $ parseSignedRelayedTransfer b
   in { mint, transfer }

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

resolveRelayRequestBody :: DecodableAsBoth -> ExceptT HTTPError AppM DecodedBody
resolveRelayRequestBody { mint: Nothing, transfer: Nothing } = throwError error400
resolveRelayRequestBody { mint: Just m, transfer: Nothing } = pure $ DecodedMint m
resolveRelayRequestBody { mint: Nothing, transfer: Just t } = pure $ DecodedTransfer t
resolveRelayRequestBody { mint: Just mint, transfer: Just transfer } = do
  isValidTransfer <- validateTransfer transfer
  pure $ 
    if isValidTransfer
    then DecodedTransfer transfer
    else DecodedMint mint

bodyToAction :: DecodedBody -> TransactionOptions NoPay -> Web3 HexString
bodyToAction (DecodedMint m) = mintRelayed m
bodyToAction (DecodedTransfer t) = transferRelayed t

postSubmitRoute :: RelayRequestBody -> { "POST" :: ExceptT HTTPError AppM HexString }
postSubmitRoute b = {
    "POST": do
      resolved <- resolveRelayRequestBody (decodeRelayRequestBody b)
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

postValidateRoute :: RelayRequestBody -> { "POST" :: ExceptT HTTPError AppM DecodedBody }
postValidateRoute b = { "POST": resolveRelayRequestBody (decodeRelayRequestBody b) }

