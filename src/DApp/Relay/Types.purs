module DApp.Relay.Types where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.ByteString (ByteString, Encoding(Hex, UTF8), fromString, singleton) as BS
import Data.Maybe (Maybe, fromJust, maybe)
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect.Exception (error)
import Network.Ethereum.Core.HexString (HexString, fromByteString, mkHexString, unHex)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Core.Signatures (Address, PrivateKey, Signature(..), signMessage)
import Network.Ethereum.Web3 (UIntN, Web3, embed)
import Network.Ethereum.Web3.Api (personal_sign)
import Network.Ethereum.Web3.Solidity (toDataBuilder)
import Network.Ethereum.Web3.Solidity.AbiEncoding (uInt256HexBuilder)
import Network.Ethereum.Web3.Solidity.Sizes (S32, S256)
import Partial.Unsafe (unsafePartialBecause)
import Record as Record
import Type.Quotient (mkQuotient)

type UnsignedRelayedMessageR r = (nonce :: UIntN S32, feeAmount :: UIntN S256, tokenURI :: String | r)
type SignedRelayedMessageR r = UnsignedRelayedMessageR (signature :: Signature | r)
newtype UnsignedRelayedMessage = UnsignedRelayedMessage (Record (UnsignedRelayedMessageR ()))
newtype SignedRelayedMessage = SignedRelayedMessage (Record (SignedRelayedMessageR ()))

type UnsignedRelayedTransferR r = (nonce :: UIntN S32, feeAmount :: UIntN S256, tokenID :: UIntN S256, destination :: Address | r)
type SignedRelayedTransferR r = UnsignedRelayedTransferR (signature :: Signature | r)
newtype UnsignedRelayedTransfer = UnsignedRelayedTransfer (Record (UnsignedRelayedTransferR ()))
newtype SignedRelayedTransfer = SignedRelayedTransfer (Record (SignedRelayedTransferR ()))

-- this is equivalent to abi.encodePacked(nonce, fee, tokenURI) (not encodePacked!!!)
-- todo: can i just somehow use the abi encoder without doing this manual offset nonsense (i.e., somehow encode a function without the bytes4 selector?)
packRelayedMessage :: UnsignedRelayedMessage -> BS.ByteString
packRelayedMessage (UnsignedRelayedMessage u) = unsafePartialBecause "were just making a bytestring from hexstrings, what could possibly go wrong" fromJust $ BS.fromString (packNonce <> packFee <> packStringOffset <> packTokenURI) BS.Hex
    where packNonce = unHex $ toDataBuilder u.nonce
          packFee = unHex $ toDataBuilder u.feeAmount
          -- we have three parameters before the string data, each blown up to uint256:
          -- nonce (32 -> 256), fee (already 256), the offset into the string (256) == 768 bits == 96 bytes
          packStringOffset = unHex $ uInt256HexBuilder (embed 96)
          packTokenURI = unHex $ toDataBuilder u.tokenURI

-- this is equivalent to abi.encode(nonce, fee, tokenID, destination) (not encodePacked!!!!)
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
  let pfx = unsafePartialBecause "we're packing a known good prefix into a bytestring" fromJust $ BS.fromString "Ethereum Signed Message:\n32" BS.UTF8
   in (BS.singleton (mkQuotient 25)) <> pfx <> bs -- 25 = 0x19 (i.e \x19)

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
  