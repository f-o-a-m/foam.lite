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
  | r
  }

testConfigToSpecConfig :: forall r
                        . Row.Lacks "nonWeb3Account" r
                        => Row.Lacks "accounts" r
                        => TestConfig r
                        -> PrivateKey
                        -> SpecConfig r
testConfigToSpecConfig tc nonWeb3Priv =
  let split = unsafePartialBecause "we expect to have multiple accounts" fromJust $ Array.uncons tc.accounts
      primaryAccount = split.head
      secondaryAccounts = split.tail
      -- todo: support getting password from cliquebait etc.
      accountPassword = const (Just "password123")
      tcWithoutAccounts = Record.delete (SProxy :: SProxy "accounts") tc
      nonWeb3Pub = privateToPublic nonWeb3Priv
      -- nonWeb3Address = privateToAddress nonWeb3Priv
      nonWeb3Address = publicToAddress nonWeb3Pub
      nonWeb3Account = { pub: nonWeb3Pub, prv: nonWeb3Priv, address: nonWeb3Address }
      specConfigParts = { primaryAccount, secondaryAccounts, accountPassword, nonWeb3Account }
   in Record.union specConfigParts tcWithoutAccounts
      