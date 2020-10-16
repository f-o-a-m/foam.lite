module DApp.Util where
  
import Prelude

import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Core.Signatures (Address)
import Network.Ethereum.Web3 (Ether, TransactionOptions(..), UIntN, Value, _from, _to, convert, defaultTransactionOptions, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S128, S256, S32, s256)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit, NoPay)
import Partial.Unsafe (unsafePartialBecause)
  
makeTxOpts :: { from :: Address, to :: Address } -> TransactionOptions NoPay
makeTxOpts { from, to } = defaultTransactionOptions
  # _from ?~ from
  # _to ?~ to

makePayEthTx :: { from :: Address, to :: Address, value :: Value Ether } -> TransactionOptions MinorUnit
makePayEthTx { from, to, value } = 
  TransactionOptions { from: Just from
                     , to: Just to
                     , value: Just $ convert value
                     , gas: Nothing
                     , gasPrice: Nothing
                     , data: Nothing
                     , nonce: Nothing
                     }

widenUIntN32 :: UIntN S32 -> UIntN S256
widenUIntN32 u32 = unsafePartialBecause "we're expanding an uint32 -> uint256" fromJust $ uIntNFromBigNumber s256 (unUIntN u32)

widenUIntN128 :: UIntN S128 -> UIntN S256
widenUIntN128 u128 = unsafePartialBecause "we're expanding an uint128 -> uint256" fromJust $ uIntNFromBigNumber s256 (unUIntN u128)