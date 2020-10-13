module UI.Component.RelayableNFT.Data where

import Prelude

import Contracts.RelayableNFT as RNFT
import Data.Maybe (Maybe(..))
import Data.Mock as Mock
import Network.Ethereum.Core.HexString (HexString)

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
  , txHash: show txHash
  , minter: Just $ show a.minter
  , owner: Nothing
  , destination: Nothing
  , relayer: show a.relayer
  , tokenID: show a.tokenID
  }
tableEntryView (Transferred txHash (RNFT.TransferredByRelay a)) =
  { _type: "Transferred"
  , txHash: show txHash
  , minter: Nothing
  , owner: Just $ show a.owner
  , destination: Just $ show a.destination
  , relayer: show a.relayer
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