module UI.Component.RelayableNFT.Types where

import Prelude

import Contracts.RelayableNFT as RNFT
import Data.Maybe (Maybe(..))
import Data.Mock as Mock
import Network.Ethereum.Core.BigNumber (BigNumber)
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Web3 (Address, unUIntN)

data TableEntry 
  = Minted HexString RNFT.MintedByRelay
  | Transferred HexString RNFT.TransferredByRelay

type TableEntryView =
  { _type :: String
  , txHash :: HexString
  , minter :: Maybe Address
  , owner :: Maybe Address
  , destination :: Maybe Address
  , relayer :: Address
  , tokenID :: BigNumber
  }

-- newtype MintedByRelay = MintedByRelay {minter :: Address,relayer :: Address,tokenID :: (UIntN (D2 :& D5 :& DOne D6))}
-- newtype TransferredByRelay = TransferredByRelay {owner :: Address,destination :: Address,relayer :: Address,tokenID :: (UIntN (D2 :& D5 :& DOne D6))}

tableEntryView
  :: TableEntry
  -> TableEntryView
tableEntryView (Minted txHash (RNFT.MintedByRelay a)) =
  { _type: "Minted"
  , txHash
  , minter: Just a.minter
  , owner: Nothing
  , destination: Nothing
  , relayer: a.relayer
  , tokenID: unUIntN a.tokenID
  }
tableEntryView (Transferred txHash (RNFT.TransferredByRelay a)) =
  { _type: "Transferred"
  , txHash: txHash
  , minter: Nothing
  , owner: Just a.owner
  , destination: Just a.destination
  , relayer: a.relayer
  , tokenID: unUIntN a.tokenID
  }

--------------------------------------------------------------------------------
-- | Mocking
--------------------------------------------------------------------------------

generateTableEntry
  :: Int
  -> TableEntry
generateTableEntry n
  | n `mod` 2 == 0 = Minted (Mock.generateTxHash n) $
      RNFT.MintedByRelay 
        { minter: Mock.generateAddress n
        , relayer: Mock.generateAddress (n + 1)
        , tokenID: Mock.generateTokenID (n + 2)
        }
  | otherwise = Transferred (Mock.generateTxHash n) $
      RNFT.TransferredByRelay
        { owner: Mock.generateAddress n
        , destination: Mock.generateAddress (n + 1)
        , relayer: Mock.generateAddress (n + 2)
        , tokenID: Mock.generateTokenID (n + 3)
        }