module DApp.Deploy.ContractConfig where

import Prelude

import Chanterelle.Internal.Types (ContractConfig)
import Contracts.FungibleToken as FT
import Contracts.RelayableNFT as RNFT
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Core.Signatures (Address)
import Network.Ethereum.Web3 (UIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256)

type Contract a =
  { deployHash :: HexString
  , deployAddress :: Address
  , deployArgs :: Record a
  }

type FungibleToken = (initialSupply :: UIntN S256)
type RelayableNFT  = (fungibleToken :: Address)

type DeployResults =
  ( fungibleToken :: Contract FungibleToken
  , relayableNFT  :: Contract RelayableNFT
  )

fungibleTokenConfig
  :: Record FungibleToken
  -> ContractConfig FungibleToken
fungibleTokenConfig args = 
  { filepath: "build/FungibleToken.json"
  , name: "FungibleToken"
  , constructor: FT.constructor
  , unvalidatedArgs: pure args
  }

relayableNFTConfig
  :: Record RelayableNFT
  -> ContractConfig RelayableNFT
relayableNFTConfig args = 
  { filepath: "build/RelayableNFT.json"
  , name: "RelayableNFT"
  , constructor: RNFT.constructor
  , unvalidatedArgs: pure args
  }