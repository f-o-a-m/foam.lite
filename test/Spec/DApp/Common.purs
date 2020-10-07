module Spec.DApp.Common (SpecConfig, testConfigToSpecConfig) where
  
import Prelude

import Chanterelle.Test (TestConfig)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (SProxy(..))
import Network.Ethereum.Web3 (Address, Provider)
import Partial.Unsafe (unsafePartialBecause)
import Prim.Row as Row
import Record as Record
  
type SpecConfig r = 
  { provider :: Provider
  , primaryAccount :: Address
  , secondaryAccounts :: Array Address
  , accountPassword :: Address -> Maybe String
  | r
  }

testConfigToSpecConfig :: forall r. Row.Lacks "accounts" r => TestConfig r -> SpecConfig r
testConfigToSpecConfig tc =
  let split = unsafePartialBecause "we expect to have multiple accounts" fromJust $ Array.uncons tc.accounts
      primaryAccount = split.head
      secondaryAccounts = split.tail
      specConfigParts = { primaryAccount, secondaryAccounts, accountPassword }
      -- todo: support getting password from cliquebait etc.
      accountPassword = const (Just "password123")
      tcWithoutAccounts = Record.delete (SProxy :: SProxy "accounts") tc
   in Record.union specConfigParts tcWithoutAccounts
      