module UI.Component.Table where

import Prelude

import Contracts.RelayableNFT as RNFT
import Data.Array ((..))
import Data.Maybe (Maybe(..), maybe)
import Data.Mock as Mock
import Halogen as H
import Halogen.HTML as HH
import Network.Ethereum.Core.HexString (HexString)
import Ocelot.Block.Table as Table
import Ocelot.HTML.Properties (css)
import UI.Style.Block.Backdrop as Backdrop
import UI.Style.Block.Documentation as Documentation

type State = Unit

data Query a
type Action = Unit

type Input = Unit

type Message = Void

component
  :: ∀ m
   . H.Component HH.HTML Query Input Message m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }
  where

    render :: State -> H.ComponentHTML Action () m
    render _ =
      HH.div_
        [ Documentation.block_
            { header: "RelaybleNFT Events Table"
            , subheader: "Table of Mint and Transfer Events for RelaybleNFT Contract"
            }
            [ Backdrop.backdrop_
              [ renderTable ]
            ]
        ]
      where
        renderTable =
          Table.table_ $
            [ renderHeader
            ]
            <> renderBody

        renderHeader =
          Table.row_
            [ Table.header_ [ HH.text "Type" ]
            , Table.header_ [ HH.text "Tx Hash" ]
            , Table.header_ [ HH.text "Minter" ]
            , Table.header_ [ HH.text "Owner" ]
            , Table.header_ [ HH.text "Destination" ]
            , Table.header_ [ HH.text "Relayer" ]
            , Table.header_ [ HH.text "Token ID" ]
            ]

        renderBody =
          Table.row_ <$> ( renderData <$> tableData )

        renderData :: ∀ p i. TableEntry -> Array (HH.HTML p i)
        renderData entry =
          let view = tableEntryView entry
          in 
            [ Table.cell  [ css "text-left" ] [ HH.text view._type ]
            , Table.cell  [ css "text-left" ] [ HH.text view.txHash ]
            , Table.cell  [ css "text-left" ] $ 
                maybe [ HH.text "N/A" ] (\a -> [ HH.text a ]) view.minter
            , Table.cell  [ css "text-left" ] $ 
                maybe [ HH.text "N/A" ] (\a -> [ HH.text a ]) view.owner
            , Table.cell  [ css "text-left" ] $ 
                maybe [ HH.text "N/A" ] (\a -> [ HH.text a ]) view.destination
            , Table.cell  [ css "text-left" ] [ HH.text view.relayer ]
            , Table.cell  [ css "text-left" ] [ HH.text view.tokenID ]
            ]

tableData :: Array TableEntry
tableData = map generateTableEntry (1 .. 10)

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