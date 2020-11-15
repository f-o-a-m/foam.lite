module MIME where

import Prelude

import Control.Alt ((<|>))
import Control.Error.Util (note)
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.MediaType.Common (applicationOctetStream, textPlain)
import Data.Tuple (Tuple(..))
import Network.Ethereum.Core.BigNumber as Eth
import Network.Ethereum.Core.HexString as Eth
import Network.Ethereum.Core.Signatures as Eth
import Type.Proxy (Proxy(..))
import Type.Trout (type (:<|>))
import Type.Trout.ContentType (class AllMimeRender, class HasMediaType, class MimeParse, class MimeRender, getMediaType, mimeParse, mimeRender)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.PathPiece (class FromPathPiece)

data PlainText
data OctetStream

instance hasMediaTypePlaintext :: HasMediaType PlainText where
  getMediaType _ = textPlain

class FromString a where
  fromString :: String -> Either String a

instance fromStringHexString :: FromString Eth.HexString where
  fromString = note "couldnt parse hex string" <<< Eth.mkHexString

instance fromStringMimeParsePlaintext :: FromString a => MimeParse String PlainText a where
  mimeParse _ = fromString

instance showMimeRenderPlaintext :: Show a => MimeRender a PlainText String where
  mimeRender _ = show

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

instance toByteStringBigNumber :: ToByteString Eth.BigNumber where
  toByteString = toByteString <<< Eth.toHexString

instance fromByteStringMimeParseOctetStream :: FromByteString a => MimeParse BS.ByteString OctetStream a where
  mimeParse _ = fromByteString

instance toByteStringMimeRenderOctetStream :: ToByteString a => MimeRender a OctetStream BS.ByteString where
  mimeRender _ = toByteString

-- Wrapper to get around orphan instances of Address, HexString etc.
newtype TroutWrapper a = TroutWrapper a
instance fromPathPieceTWHexString :: FromPathPiece (TroutWrapper Eth.HexString) where
  fromPathPiece s = TroutWrapper <$> fromString s
instance fromPathPieceTWAddress :: FromPathPiece (TroutWrapper Eth.Address) where
  fromPathPiece s = TroutWrapper <$> (fromString s >>= (note "hex string was not a valid addres" <<< Eth.mkAddress))