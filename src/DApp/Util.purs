module DApp.Util where
  
import Prelude

import Data.Lens ((?~))
import Data.Maybe (Maybe(..))
import Network.Ethereum.Core.Signatures (Address)
import Network.Ethereum.Web3 (Ether, TransactionOptions(..), Value, _from, _to, convert, defaultTransactionOptions)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit, NoPay)
  
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
