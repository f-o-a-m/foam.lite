module Spec.DApp.Relay where

import Prelude

import Control.Monad.Gen (chooseInt)
import DApp.Message (DAppMessage)
import DApp.Relay (SignedRelayedMessage(..), SignedRelayedTransfer(..), packSignedRelayedMessage, packSignedRelayedTransfer, parseSignedRelayedMessage, parseSignedRelayedTransfer)
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Enum (enumFromTo)
import Data.Foldable (for_)
import Data.List as List
import Data.Maybe (maybe)
import Data.Tuple (fst)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.HexString (HexString, fromByteString, toBigNumber)
import Network.Ethereum.Core.Signatures (Address, Signature(..), mkAddress)
import Network.Ethereum.Web3 (class KnownSize, DLProxy(..), UIntN, sizeVal, uIntNFromBigNumber)
import Test.QuickCheck (arbitrary, randomSeed)
import Test.QuickCheck.Gen (Gen, listOf, runGen)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

relaySpec :: Spec Unit
relaySpec = describe "DApp.Relay" do
  relaySpec' "Message" genSignedRelayedMessage packSignedRelayedMessage parseSignedRelayedMessage
  relaySpec' "Transfer" genSignedRelayedTransfer packSignedRelayedTransfer parseSignedRelayedTransfer

relaySpec' :: forall a. Eq a => Show a => String -> (Gen a) -> (a -> BS.ByteString) -> (BS.ByteString -> Either String a) -> Spec Unit
relaySpec' name gen pack parse = it ("can pack/unpacked SignedRelayed" <> name) do
  for_ (enumFromTo 1 100 :: Array Int) $ \_ -> do
      msg <- do
        newSeed <- liftEffect randomSeed
        pure $ fst $ runGen gen $ { newSeed, size: 100 }
      let encoded = pack msg
          decoded = parse encoded
      decoded `shouldEqual` Right msg

arbHexStringWithByteLength :: Int -> Gen HexString
arbHexStringWithByteLength len = fromByteString <<< BS.pack <<< List.toUnfoldable <$> listOf len arbitrary

arbSignature :: Gen Signature
arbSignature = do
  r <- arbHexStringWithByteLength 32
  s <- arbHexStringWithByteLength 32
  v <- chooseInt 27 28
  pure $ Signature { r, s, v }

arbUIntN :: forall s. KnownSize s => Gen (UIntN s)
arbUIntN = do
  let dlProxy = DLProxy :: DLProxy s 
      numBytes = (sizeVal dlProxy) / 8
  bn <- uIntNFromBigNumber dlProxy <<< toBigNumber <$> arbHexStringWithByteLength numBytes
  maybe arbUIntN pure bn

arbAddress :: Gen Address
arbAddress = do
  hex <- arbHexStringWithByteLength 20
  maybe arbAddress pure (mkAddress hex)

genSignedRelayedMessage :: Gen SignedRelayedMessage
genSignedRelayedMessage = do
  signature <- arbSignature
  nonce <- arbUIntN
  feeAmount <- arbUIntN
  tokenURI <- show <$> (arbitrary :: Gen DAppMessage)
  pure $ SignedRelayedMessage { signature, nonce, feeAmount, tokenURI }

genSignedRelayedTransfer :: Gen SignedRelayedTransfer
genSignedRelayedTransfer = do
  signature <- arbSignature
  nonce <- arbUIntN
  feeAmount <- arbUIntN
  tokenID <- arbUIntN
  destination <- arbAddress
  pure $ SignedRelayedTransfer { signature, nonce, feeAmount, tokenID, destination }