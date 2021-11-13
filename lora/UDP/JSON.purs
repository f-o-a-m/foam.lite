module Lora.UDP.PushDataJSON (PushDataJSON(..), ReceivedPacket(..), Base64Encoded(..), decodeBase64) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut as A 
import Data.Argonaut ((.:?))
import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Data.String.Base64 (decode)

newtype PushDataJSON = PushDataJSON { rxpk :: Maybe (Array ReceivedPacket) }
-- derive newtype instance decodeJsonPushDataJSON :: A.DecodeJson PushDataJSON
instance decodeJsonPushDataJSON :: A.DecodeJson PushDataJSON where
  decodeJson j  = do
    obj <- A.decodeJson j
    rxpk <- obj .:? "rxpk"
    pure $ PushDataJSON { rxpk }
derive newtype instance encodeJsonPushDataJSON :: A.EncodeJson PushDataJSON
derive instance genericPushDataJSON :: Generic PushDataJSON _
instance showPushDataJSON :: Show PushDataJSON where
  show = genericShow

 -- ReceivedPacket 
newtype ReceivedPacket = ReceivedPacket
  {
  -- time :: String
  -- , tmms :: Int
  -- , tmst :: Int
  -- , modu :: String
  -- , datr :: DataRate
   data :: Base64Encoded
  }
derive newtype instance decodeJsonReceivedPacket :: A.DecodeJson ReceivedPacket
derive newtype instance encodeJsonReceivedPacket :: A.EncodeJson ReceivedPacket
derive instance genericReceivedPacket :: Generic ReceivedPacket _
instance showReceivedPacket :: Show ReceivedPacket where
  show = genericShow

 -- Base64Encoded
newtype Base64Encoded = Base64Encoded String
derive newtype instance decodeJsonBase64Encoded :: A.DecodeJson Base64Encoded
derive newtype instance encodeJsonBase64Encoded :: A.EncodeJson Base64Encoded
derive instance genericBase64Encoded :: Generic Base64Encoded _
instance showBase64Encoded :: Show Base64Encoded where
  show = genericShow
decodeBase64 :: Base64Encoded -> Either Error String
decodeBase64 (Base64Encoded a) = decode a

 -- DataRate
data DataRate = LoraDataRate String
              | FSK Int
derive instance genericDataRate :: Generic DataRate _
instance showDataRate :: Show DataRate where
  show = genericShow
instance decodeJsonDataRate :: A.DecodeJson DataRate where
  decodeJson j = decodeLoraDataRate <|> decodeFSKRate <|> reportFailure
    where decodeLoraDataRate = LoraDataRate <$> A.decodeJson j
          decodeFSKRate = FSK <$> A.decodeJson j
          reportFailure = Left $ A.TypeMismatch "Expected a String or an Int"
instance encodeJsonDataRate :: A.EncodeJson DataRate where
  encodeJson (LoraDataRate s) = A.encodeJson s
  encodeJson (FSK i) = A.encodeJson i