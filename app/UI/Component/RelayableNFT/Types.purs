module UI.Component.RelayableNFT.Types where

import Prelude

import Contracts.RelayableNFT as RNFT
import Data.Maybe (Maybe(..))
import Data.Mock as Mock
import Network.Ethereum.Core.HexString (HexString)
import UI.Utils (formatAddress, formatHexString)

data TableEntry 
  = Minted HexString RNFT.MintedByRelay
  | Transferred HexString RNFT.TransferredByRelay

-- newtype MintedByRelay = MintedByRelay {minter :: Address,relayer :: Address,tokenID :: (UIntN (D2 :& D5 :& DOne D6))}
-- newtype TransferredByRelay = TransferredByRelay {owner :: Address,destination :: Address,relayer :: Address,tokenID :: (UIntN (D2 :& D5 :& DOne D6))}

tableEntryView
  :: TableEntry
  -> { _type :: String
     , txHash :: String
     , minter :: Maybe String
     , owner :: Maybe String
     , destination :: Maybe String
     , relayer :: String
     , tokenID :: String
     }
tableEntryView (Minted txHash (RNFT.MintedByRelay a)) =
  { _type: "Minted"
  , txHash: formatHexString txHash
  , minter: Just <<< formatAddress $ a.minter
  , owner: Nothing
  , destination: Nothing
  , relayer: formatAddress a.relayer
  , tokenID: show a.tokenID
  }
tableEntryView (Transferred txHash (RNFT.TransferredByRelay a)) =
  { _type: "Transferred"
  , txHash: formatHexString txHash
  , minter: Nothing
  , owner: Just <<< formatAddress $ a.owner
  , destination: Just <<< formatAddress $ a.destination
  , relayer: formatAddress a.relayer
  , tokenID: show a.tokenID
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