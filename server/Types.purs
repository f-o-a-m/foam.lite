module Types where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import DApp.Relay (SignedRelayedMessage, SignedRelayedTransfer)
import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Effect.Aff (Aff)
import Network.Ethereum.Core.BigNumber (BigNumber)
import Network.Ethereum.Core.Signatures (Address)
import Network.Ethereum.Web3 (HexString, Provider, Web3)

type AppEnv = {
  chainID :: BigNumber,
  addresses :: { fungibleToken :: Address, relayableNFT :: Address, primaryAccount :: Address },
  provider :: Provider,
  relayActions :: { doMintRelayed :: SignedRelayedMessage -> Web3 HexString, doTransferRelayed :: SignedRelayedTransfer -> Web3 HexString }
}

data AppError = Other String

derive instance genericAppError :: Generic AppError _
instance encodeJsonAppError :: EncodeJson AppError where
  encodeJson = genericEncodeJson

instance showAppError :: Show AppError where
  show (Other s) = s

type AppM = ReaderT AppEnv Aff

newtype AppResult a = AppResult (Either AppError a)

derive instance genericAppResult :: Generic (AppResult a) _

runAppM :: forall a. AppEnv -> AppM a -> Aff a
runAppM env = flip runReaderT env