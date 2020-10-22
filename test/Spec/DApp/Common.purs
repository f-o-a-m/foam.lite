module Spec.DApp.Common (SpecConfig, testConfigToSpecConfig) where
  
import Prelude

import Chanterelle.Test (TestConfig)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (SProxy(..))
import Network.Ethereum.Core.Signatures (PrivateKey, PublicKey, privateToPublic, publicToAddress)
import Network.Ethereum.Web3 (Address, Provider)
import Partial.Unsafe (unsafePartialBecause)
import Prim.Row as Row
import Record as Record
  
type SpecConfig r = 
  { provider :: Provider
  , primaryAccount :: Address
  , secondaryAccounts :: Array Address
  , accountPassword :: Address -> Maybe String
  , nonWeb3Account :: { pub :: PublicKey, prv :: PrivateKey, address :: Address }
  , secondNonWeb3Account :: { pub :: PublicKey, prv :: PrivateKey, address :: Address }
  | r
  }

testConfigToSpecConfig :: forall r
                        . Row.Lacks "nonWeb3Account" r
                        => Row.Lacks "accounts" r
                        => TestConfig r
                        -> PrivateKey
                        -> PrivateKey
                        -> SpecConfig r
testConfigToSpecConfig tc nonWeb3Priv nonWeb3Priv2 =
  let split = unsafePartialBecause "we expect to have multiple accounts" fromJust $ Array.uncons tc.accounts
      primaryAccount = split.head
      secondaryAccounts = split.tail
      -- todo: support getting password from cliquebait etc.
      accountPassword = const (Just "password123")
      tcWithoutAccounts = Record.delete (SProxy :: SProxy "accounts") tc
      mkNonWeb3Account prv =
        let pub = privateToPublic prv
            address = publicToAddress pub
         in { pub, prv, address }
      nonWeb3Account = mkNonWeb3Account nonWeb3Priv
      secondNonWeb3Account = mkNonWeb3Account nonWeb3Priv2
      specConfigParts = { primaryAccount, secondaryAccounts, accountPassword, nonWeb3Account, secondNonWeb3Account }
   in Record.union specConfigParts tcWithoutAccounts
      