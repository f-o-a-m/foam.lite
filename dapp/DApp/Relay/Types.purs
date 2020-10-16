module DApp.Relay.Types where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Array (replicate)
import Data.ByteString (ByteString, Encoding(Hex, UTF8), fromString, fromUTF8, length, singleton, toString, toUTF8) as BS
import Data.Either (Either)
import Data.EitherR (fmapL)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe, fromJust, maybe)
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect.Exception (error)
import Network.Ethereum.Core.BigNumber as BigNumber
import Network.Ethereum.Core.HexString (HexString, fromByteString, mkHexString, toBigNumber, toByteString, unHex)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Core.Signatures (Address, PrivateKey, Signature(..), mkAddress, signMessage, unAddress)
import Network.Ethereum.Web3 (class KnownSize, DLProxy(..), UIntN, Web3, embed, sizeVal, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3.Api (personal_sign)
import Network.Ethereum.Web3.Solidity (toDataBuilder)
import Network.Ethereum.Web3.Solidity.AbiEncoding (uInt256HexBuilder)
import Network.Ethereum.Web3.Solidity.AbiEncoding as AbiEncoding
import Network.Ethereum.Web3.Solidity.Sizes (S128, S32, s8)
import Partial.Unsafe (unsafePartialBecause)
import Record as Record
import Text.Parsing.Parser (ParserT, fail, runParserT)
import Type.Quotient (mkQuotient)

type UnsignedRelayedMessageR r = (nonce :: UIntN S32, feeAmount :: UIntN S128, tokenURI :: String | r)
type SignedRelayedMessageR r = UnsignedRelayedMessageR (signature :: Signature | r)
newtype UnsignedRelayedMessage = UnsignedRelayedMessage (Record (UnsignedRelayedMessageR ()))
newtype SignedRelayedMessage = SignedRelayedMessage (Record (SignedRelayedMessageR ()))

instance showSignedRelayedMessage :: Show SignedRelayedMessage where
  show (SignedRelayedMessage s) = show s

derive instance eqSignedRelayedMessage :: Eq SignedRelayedMessage

instance encodeJsonSignedRelayedMessage :: EncodeJson SignedRelayedMessage where
  encodeJson (SignedRelayedMessage s) = 
    let (Signature sig) = s.signature 
     in encodeJson
          {
            signature: { r: sig.r, s: sig.s, v: sig.v },
            nonce: unUIntN s.nonce,
            feeAmount: unUIntN s.feeAmount,
            tokenURI: s.tokenURI
          }

type UnsignedRelayedTransferR r = (nonce :: UIntN S32, feeAmount :: UIntN S128, tokenID :: UIntN S32, destination :: Address | r)
type SignedRelayedTransferR r = UnsignedRelayedTransferR (signature :: Signature | r)
newtype UnsignedRelayedTransfer = UnsignedRelayedTransfer (Record (UnsignedRelayedTransferR ()))
newtype SignedRelayedTransfer = SignedRelayedTransfer (Record (SignedRelayedTransferR ()))

instance showSignedRelayedTransfer :: Show SignedRelayedTransfer where
  show (SignedRelayedTransfer s) = show s

derive instance eqSignedRelayedTransfer :: Eq SignedRelayedTransfer

instance encodeJsonSignedRelayedTransfer :: EncodeJson SignedRelayedTransfer where
  encodeJson (SignedRelayedTransfer s) = 
    let (Signature sig) = s.signature 
     in encodeJson
          {
            signature: { r: sig.r, s: sig.s, v: sig.v },
            nonce: unUIntN s.nonce,
            feeAmount: unUIntN s.feeAmount,
            destination: s.destination
          }

packSignature :: Signature -> BS.ByteString
packSignature (Signature { r, s, v }) = packR <> packS <> packV
  where packR = toByteString r
        packS = toByteString s
        packV = BS.singleton $ mkQuotient v

packUIntNToHex :: forall s. KnownSize s => UIntN s -> String
packUIntNToHex n =
  let totalChars = (sizeVal (DLProxy :: DLProxy s)) / 4 -- one hex char == 4 bits (e.g., s32 == 4 bytes == 8 hex chars)
      hex = BigNumber.toString BigNumber.hexadecimal $ unUIntN n
      neededChars = totalChars - (String.length hex)
      padding = String.fromCodePointArray $ replicate neededChars (String.codePointFromChar '0')
   in padding <> hex

packStringToHex :: String -> String
packStringToHex s = 
  let hex = BS.toString (BS.toUTF8 s) BS.Hex
      len = String.length hex / 2 -- 2 hex chars per byte
      encodedLen = packUIntNToHex (unsafePartialBecause "we're willingly limiting our length to 1 byte" fromJust $ uIntNFromBigNumber s8 (embed $ len))
   in encodedLen <> hex

parseStringFromHex :: forall m. Monad m => ParserT HexString m String
parseStringFromHex = do
  let numLengthHexChars = sizeVal s8 / 4 
  maybeLen <- (uIntNFromBigNumber s8 <<< toBigNumber <$> AbiEncoding.take numLengthHexChars)
  len <- maybe (fail "Couldn't parse uint8 length") pure $ maybeLen
  let numStringHexChars = (BigNumber.unsafeToInt $ unUIntN len) * 2
  (BS.fromUTF8 <<< toByteString) <$> AbiEncoding.take numStringHexChars

parseUIntNFromHex :: forall m s. Monad m => KnownSize s => ParserT HexString m (UIntN s)
parseUIntNFromHex = do
  let (proxy :: DLProxy s) = DLProxy
      numChars = (sizeVal proxy) / 4  -- one hex char = 4 bits, (e.g., s256 == 32 bytes == 64 hex chars)
  maybeUInt <- (uIntNFromBigNumber proxy <<< toBigNumber <$> AbiEncoding.take numChars)
  maybe (fail "Couldn't fit BigNumber into desired uintN") pure $ maybeUInt

parseSignature :: forall m. Monad m => ParserT HexString m Signature
parseSignature = do
  r <- AbiEncoding.take 64
  s <- AbiEncoding.take 64
  v <- toBigNumber <$> AbiEncoding.take 2
  pure $ Signature { r, s, v: BigNumber.unsafeToInt v }

parseAddressHex :: forall m. Monad m => ParserT HexString m Address
parseAddressHex = do
  addr <- mkAddress <$> AbiEncoding.take 40
  maybe (fail "Couldn't parse address from 20 bytes") pure $ addr

-- this is for packing a *signed* message, e.g., for broadcast over the air. its more like an encodePacked but more aggressive (with a 1-byte length prefix for tokenURI)
packSignedRelayedMessage :: SignedRelayedMessage -> BS.ByteString
packSignedRelayedMessage (SignedRelayedMessage { signature, nonce, feeAmount, tokenURI }) = packedSignature <>
  (unsafePartialBecause "were just making a bytestring from hexstrings, what could possibly go wrong" fromJust $ 
    BS.fromString (packNonce <> packFee <> packTokenURI) BS.Hex)
  where packedSignature = packSignature signature
        packNonce = packUIntNToHex nonce
        packFee = packUIntNToHex feeAmount
        packTokenURI = packStringToHex tokenURI

parseSignedRelayedMessage :: BS.ByteString -> Either String SignedRelayedMessage
parseSignedRelayedMessage bs = fmapL show ret
  where (Identity ret) = runParserT (fromByteString bs) do
          signature <- parseSignature
          nonce <- parseUIntNFromHex
          feeAmount <- parseUIntNFromHex
          tokenURI <- parseStringFromHex
          pure $ SignedRelayedMessage { signature, nonce, feeAmount, tokenURI }

-- this is for a packing a *signed* transfer, e.g., for broadcast over the air. this is more like abi.encodePacked
packSignedRelayedTransfer :: SignedRelayedTransfer -> BS.ByteString
packSignedRelayedTransfer (SignedRelayedTransfer { signature, nonce, feeAmount, tokenID, destination }) = packedSignature <> 
  (unsafePartialBecause "were just making a bytestring from hexstrings, what could possibly go wrong" fromJust $
    BS.fromString (packedNonce <> packedFee <> packedTokenID <> packedDestination) BS.Hex)
  where packedSignature = packSignature signature
        packedNonce = packUIntNToHex nonce
        packedFee = packUIntNToHex feeAmount
        packedTokenID = packUIntNToHex tokenID
        packedDestination = unHex (unAddress destination)

parseSignedRelayedTransfer :: BS.ByteString -> Either String SignedRelayedTransfer
parseSignedRelayedTransfer bs = fmapL show ret
  where (Identity ret) = runParserT (fromByteString bs) do
          signature <- parseSignature
          nonce <- parseUIntNFromHex
          feeAmount <- parseUIntNFromHex
          tokenID <- parseUIntNFromHex
          destination <- parseAddressHex
          pure $ SignedRelayedTransfer { signature, nonce, feeAmount, tokenID, destination }
  

-- this is equivalent to abi.encode(nonce, fee, tokenURI) (not encodePacked!!!)
-- todo: can i just somehow use the abi encoder without doing this manual offset nonsense (i.e., somehow encode a function without the bytes4 selector?)
packRelayedMessage :: UnsignedRelayedMessage -> BS.ByteString
packRelayedMessage (UnsignedRelayedMessage u) = unsafePartialBecause "were just making a bytestring from hexstrings, what could possibly go wrong" fromJust $ BS.fromString (packNonce <> packFee <> packStringOffset <> packTokenURI) BS.Hex
    where packNonce = unHex $ toDataBuilder u.nonce
          packFee = unHex $ toDataBuilder u.feeAmount
          -- we have three parameters before the string data, each blown up to uint256:
          -- nonce (32 -> 256), fee (already 256), the offset into the string (256) == 768 bits == 96 bytes
          packStringOffset = unHex $ uInt256HexBuilder (embed 96)
          packTokenURI = unHex $ toDataBuilder u.tokenURI

-- this is equivalent to abi.encode(nonce, fee, tokenID, destination) (not encodePacked!!!)
-- todo: can i just somehow use the abi encoder without doing this manual offset nonsense (i.e., somehow encode a function without the bytes4 selector?)
packRelayedTransfer :: UnsignedRelayedTransfer -> BS.ByteString
packRelayedTransfer (UnsignedRelayedTransfer u) = unsafePartialBecause "were just making a bytestring from hexstrings, what could possibly go wrong" fromJust $ BS.fromString (packNonce <> packFee <> packTokenID <> packDestination) BS.Hex
    where packNonce = unHex $ toDataBuilder u.nonce
          packFee = unHex $ toDataBuilder u.feeAmount
          packTokenID = unHex $ toDataBuilder u.tokenID
          packDestination = unHex $ toDataBuilder u.destination

hashRelayedMessage :: UnsignedRelayedMessage -> BS.ByteString
hashRelayedMessage = keccak256 <<< packRelayedMessage

hashRelayedTransfer :: UnsignedRelayedTransfer -> BS.ByteString
hashRelayedTransfer = keccak256 <<< packRelayedTransfer

toEthSignedMessage :: BS.ByteString -> BS.ByteString
toEthSignedMessage bs =
  let x19 = BS.singleton $ mkQuotient 25
      pfx = unsafePartialBecause "we're packing a known good prefix into a bytestring" fromJust $ BS.fromString "Ethereum Signed Message:\n" BS.UTF8
      lenStr = unsafePartialBecause "we're doing a BS.fromString <<< show Int" fromJust <<< flip BS.fromString BS.UTF8 <<< show <<< BS.length
   in x19 <> pfx <> lenStr bs <> bs -- 25 = 0x19 (i.e \x19)

-- todo: this probably needs to sign keccak256("\x19Ethereum Signed Message:\n32" + <hashRelayedMessage>) like personal_sign does...
signRelayedMessage :: PrivateKey -> UnsignedRelayedMessage -> SignedRelayedMessage
signRelayedMessage prk urm@(UnsignedRelayedMessage u) =
  let Signature signature = signMessage prk (keccak256 <<< toEthSignedMessage $ hashRelayedMessage urm)
      fixedSignature = Signature { r: signature.r, s: signature.s, v: signature.v + 27 } -- ethereum sigs use v=27 or v=28 instead of 0/1
      in SignedRelayedMessage (Record.insert (SProxy :: SProxy "signature") fixedSignature u)

-- todo: this probably needs to sign keccak256("\x19Ethereum Signed Message:\n32" + <hashRelayedMessage>) like personal_sign does...
signRelayedTransfer :: PrivateKey -> UnsignedRelayedTransfer -> SignedRelayedTransfer
signRelayedTransfer prk urt@(UnsignedRelayedTransfer u) =
  let Signature signature = signMessage prk (keccak256 <<< toEthSignedMessage $ hashRelayedTransfer urt)
      fixedSignature = Signature { r: signature.r, s: signature.s, v: signature.v + 27 } -- ethereum sigs use v=27 or v=28 instead of 0/1
      in SignedRelayedTransfer (Record.insert (SProxy :: SProxy "signature") fixedSignature u)

signWeb3 :: HexString -> Address -> Maybe String -> Web3 Signature
signWeb3 hxs addr password = do
  rawSignatureString <- unHex <$> personal_sign hxs addr password
  let rSplit = String.splitAt 64 rawSignatureString -- first 64 nibbles == 32 bytes of `r`, remainder = hex of `s` and `v`
      rHex = rSplit.before
      sSplit = String.splitAt 64 rSplit.after       -- first 64 nibbles of remainder == 32 bytes of `s`, remainder == hex of `v`
      sHex = sSplit.before 
      vHex = sSplit.after
  r <- maybe (throwError $ error "couldn't extract the `r` part of the signature") pure $ mkHexString rHex
  s <- maybe (throwError $ error "couldn't extract the `s` part of the signature") pure $ mkHexString sHex
  v <- case vHex of
         "1b" -> pure 27
         "1B" -> pure 27
         "1c" -> pure 28
         "1C" -> pure 28
         x -> throwError $ error $ "Expected `v` to be 27 or 28 (hex 1b/1c), but got: \"" <> x <> "\""
  pure $ Signature { r, s, v }
  
signRelayedMessageWeb3 :: Address -> Maybe String -> UnsignedRelayedMessage -> Web3 SignedRelayedMessage
signRelayedMessageWeb3 addr password urm@(UnsignedRelayedMessage u) = do
  let hashHex = fromByteString $ hashRelayedMessage urm
  signature <- signWeb3 hashHex addr password
  pure $ SignedRelayedMessage (Record.insert (SProxy :: SProxy "signature") signature u)

signRelayedTransferWeb3 :: Address -> Maybe String -> UnsignedRelayedTransfer -> Web3 SignedRelayedTransfer
signRelayedTransferWeb3 addr password urt@(UnsignedRelayedTransfer t) = do
  let hashHex = fromByteString $ hashRelayedTransfer urt
  signature <- signWeb3 hashHex addr password
  pure $ SignedRelayedTransfer (Record.insert (SProxy :: SProxy "signature") signature t)
  