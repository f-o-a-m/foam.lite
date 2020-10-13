module Data.Mock where

import Prelude

import Data.Array ((..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.String as String
import Network.Ethereum.Core.HexString (toBigNumber)
import Network.Ethereum.Core.Signatures (Address, mkAddress)
import Network.Ethereum.Web3 (BigNumber, HexString, UIntN, mkHexString, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Partial.Unsafe (unsafeCrashWith)

generateAddress
  :: Int
  -> Address
generateAddress i = mkAddress' $ "0x" <> 
  (String.take 40 $ foldMap (show <<< const i) (1 .. 40))

generateTokenID
  :: Int
  -> UIntN S256
generateTokenID n = 
  let nStr = show n
      paddedNStr = if String.length nStr `mod` 2 == 0 then nStr else "0" <> nStr
  in mkUInt256 $ mkBigNumber' paddedNStr

mkAddress' :: String -> Address
mkAddress' addr = case mkAddress =<< mkHexString addr of
  Nothing -> unsafeCrashWith $ "Invalid Address: " <> addr
  Just addr' -> addr'

mkHexString' :: String -> HexString
mkHexString' hx = case mkHexString hx of
  Nothing -> unsafeCrashWith $ "Invalid HexString: " <> hx
  Just hx' -> hx'

mkUInt256 :: BigNumber -> UIntN S256
mkUInt256 bn = case uIntNFromBigNumber s256 bn of
  Nothing -> unsafeCrashWith $ "Invalid uint256: " <> show bn
  Just n -> n

mkBigNumber' :: String -> BigNumber
mkBigNumber' bn = case toBigNumber <$> mkHexString bn of
  Nothing -> unsafeCrashWith $ "Invalid HexString: " <> bn
  Just bn' -> bn'