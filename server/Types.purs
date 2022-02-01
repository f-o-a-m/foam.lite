module Types where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import DApp.Support (AppEnv)
import Effect.Aff (Aff)

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