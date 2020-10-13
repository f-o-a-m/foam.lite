module UI.Component.RelayableNFT.Table where

import Prelude

import Data.Array ((..))
import Data.Maybe (maybe)
import Halogen as H
import Halogen.HTML as HH
import Ocelot.Block.Table as Table
import Ocelot.HTML.Properties (css)
import UI.Component.RelayableNFT.Data (TableEntry, generateTableEntry, tableEntryView)
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