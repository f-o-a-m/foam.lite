module DApp.Util where
  
import Prelude
import Data.Lens ((?~))
import Network.Ethereum.Core.Signatures (Address)
import Network.Ethereum.Web3 (TransactionOptions, _from, _to, defaultTransactionOptions)
import Network.Ethereum.Web3.Types (NoPay)
  
makeTxOpts :: { from :: Address, to :: Address } -> TransactionOptions NoPay
makeTxOpts { from, to } = defaultTransactionOptions
  # _from ?~ from
  # _to ?~ to