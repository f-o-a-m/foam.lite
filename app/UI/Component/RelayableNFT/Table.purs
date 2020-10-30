module UI.Component.RelayableNFT.Table where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Array ((:), (..))
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Ocelot.Block.Table as Table
import Ocelot.HTML.Properties (css)
import UI.Component.RelayableNFT.Types (TableEntry, generateTableEntry, tableEntryView)
import UI.Monad (AppEnv)
import UI.Style.Block.Backdrop as Backdrop
import UI.Style.Block.Documentation as Documentation

_nftTable :: SProxy "nftTable"
_nftTable = SProxy

type State = Array TableEntry

data Query a
  = InsertNewTableEntry TableEntry a

data Action

type Input = Unit

type Message = Void

component
  :: ∀ m.
     MonadAff m
  => MonadAsk AppEnv m
  => H.Component HH.HTML Query Input Message m
component =
  H.mkComponent
    { initialState: const $ map generateTableEntry (1 .. 2)
    , render
    , eval
    }
  where

    render :: forall s. State -> H.ComponentHTML Action s m
    render s =
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
          Table.row_ <$> ( renderData <$> s )

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


    eval :: forall i . 
            H.HalogenQ Query Action i
         ~> H.HalogenM State Action () Message m
    eval = H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , initialize = Nothing
      }
      where
        handleQuery :: forall a.  Query a -> H.HalogenM State Action () Message m (Maybe a)
        handleQuery = case _ of
          InsertNewTableEntry entry next -> do
            st <- H.get
            H.put $ entry : st 
            pure $ Just next