module MIME where

import Prelude

import Control.Alt ((<|>))
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.MediaType.Common (applicationOctetStream, textPlain)
import Data.Tuple (Tuple(..))
import Network.Ethereum.Core.HexString as Eth
import Type.Proxy (Proxy(..))
import Type.Trout (type (:<|>))
import Type.Trout.ContentType (class AllMimeRender, class HasMediaType, class MimeParse, class MimeRender, getMediaType, mimeParse, mimeRender)
import Type.Trout.ContentType.JSON (JSON)

data PlainText
data OctetStream

instance hasMediaTypePlaintext :: HasMediaType PlainText where
  getMediaType _ = textPlain

class FromString a where
  fromString :: String -> Either String a

instance fromStringHexString :: FromString Eth.HexString where
  fromString = maybe (Left "couldnt parse hex string") Right <<< Eth.mkHexString

instance fromStringMimeParsePlaintext :: FromString a => MimeParse String PlainText a where
  mimeParse _ = fromString

instance showMimeRenderPlaintext :: Show a => MimeRender a PlainText String where
  mimeRender _ = show

instance showAllMimeRenderPlaintext :: Show a => AllMimeRender a PlainText String where
  allMimeRender p x = pure (Tuple (getMediaType p) (mimeRender p x))

instance hasMediaTypeOctetStream :: HasMediaType OctetStream where
  getMediaType _ = applicationOctetStream

class FromByteString a where
  fromByteString :: BS.ByteString -> Either String a

instance fromByteStringIdentity :: FromByteString BS.ByteString where
  fromByteString = pure

class ToByteString a where
  toByteString :: a -> BS.ByteString

instance toByteStringIdentity :: ToByteString BS.ByteString where
  toByteString = identity

instance toByteStringHexString :: ToByteString Eth.HexString where
  toByteString = Eth.toByteString

instance fromByteStringMimeParseOctetStream :: FromByteString a => MimeParse BS.ByteString OctetStream a where
  mimeParse _ = fromByteString

instance toByteStringMimeRenderOctetStream :: ToByteString a => MimeRender a OctetStream BS.ByteString where
  mimeRender _ = toByteString

instance toByteStringAllMimeRenderOctetStream :: ToByteString a => AllMimeRender a OctetStream BS.ByteString where
  allMimeRender p x = pure (Tuple (getMediaType p) (mimeRender p x))