module UI.Component.RelayableNFT.Table where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader (class MonadAsk)
import Data.Array (length, null, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.Ethereum.Core.BigNumber (embed)
import Ocelot.HTML.Properties (css)
import UI.Component.Logging.Icons as Icons
import UI.Component.RelayableNFT.Types (TableEntry, tableEntryView)
import UI.Config (BlockExplorer, blockExplorerAddressLink, blockExplorerTxLink)
import UI.Monad (AppEnv)

foreign import ellipsize :: Int -> Int -> String -> String

_nftTable :: SProxy "nftTable"
_nftTable = SProxy

data BackfillStatus = 
    BackfillNeverStarted
  | BackfillRunning
  | BackfillFinished
  | BackfillErrored String

type State = { entries :: Array TableEntry
             , historicalEntries :: Array TableEntry
             , blockExplorer :: Maybe BlockExplorer
             , backfillStatus :: BackfillStatus
             }

data Query a
  = InsertNewTableEntry TableEntry a
  | InsertHistoricalTableEntry TableEntry a
  | BackfillStatusUpdated BackfillStatus a
  | BlockExplorerUpdated (Maybe BlockExplorer) a

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
    { initialState: const $ { entries: [], historicalEntries: [], backfillStatus: BackfillNeverStarted, blockExplorer: Nothing }
    , render
    , eval
    }
  where

    render :: forall s. State -> H.ComponentHTML Action s m
    render { entries, historicalEntries, backfillStatus, blockExplorer } = 
      HH.div
        [ css "flex w-full px-8 md:p-8 lg:pt-20 text-center" ]
        [
          HH.table
          [ css "w-full" ]
          ([ tableHead ] <> renderTableEntries)
        ]
      where
        tableHeadings = ["Type", "Tx Hash", "Minter", "Owner", "Token"]
        tableHead = 
          HH.thead
          []
          [ HH.tr
            [ css "hidden sm:table-row line-height-event-log border-b border-white last:border-b-0" ]
            ((\th -> HH.th [css "font-medium line-height-event-log"] [ HH.text th ]) <$> tableHeadings)
          ]

        responsiveHashString str =
          HH.div [ css "contents" ]
          [ HH.span [ css "inline md:hidden" ] [HH.text $ ellipsize 2 2 str]
          , HH.span [ css "hidden md:inline xl:hidden" ] [HH.text $ ellipsize 3 3 str]
          , HH.span [ css "hidden xl:inline xxl:hidden" ] [HH.text $ ellipsize 4 4 str]
          , HH.span [ css "hidden xxl:inline 3xl:hidden" ] [HH.text $ "0x" <> ellipsize 10 10 str]
          , HH.span [ css "hidden 3xl:inline 4xl:hidden" ] [HH.text $ "0x" <> ellipsize 14 14 str]
          , HH.span [ css "hidden 4xl:inline" ] [HH.text $ "0x" <> ellipsize 20 20 str]
          ]

        txHashLink txHash = responsiveLink (show txHash) $ blockExplorerTxLink blockExplorer txHash
        addressLink addr = responsiveLink (show addr) $ blockExplorerAddressLink blockExplorer addr
              
        responsiveLink text href = case href of
          Nothing -> HH.span [ css "inline-block" ] [ responsiveHashString text ]
          Just href' ->
              HH.a
              [ HP.href href', HP.target "_blank", css "underline" ]
              [ responsiveHashString text, Icons.externalLink 4 ["inline-block", "-mb-1"] [] ]

        tokenIDToName tid = "Token #" <> show tid
        fullWidthCell = HP.colSpan (length tableHeadings)
        tokenImage size tokenID extras =
          HH.img
          [ HP.alt $ tokenIDToName tokenID
          , HP.title $ tokenIDToName tokenID
          , HP.src ("/static-token-images/" <> show (tokenID `mod` (embed 3) + (embed 1)) <> ".png")
          , css ("w-" <> show size <> " h-" <> show size <> " inline-block" <> " " <> extras)
          ]
        justOrNA = fromMaybe (HH.text "N/A")

        renderTableEntries = renderNewTableEntries <> historicalEntriesInfo <> renderHistoricalTableEntries

        renderNewTableEntries = tableEntry <$> entries
        renderHistoricalTableEntries = tableEntry <$> historicalEntries
        historicalEntriesInfo =
          let bfState@{noNew, noOld, bfs} = { noNew: null entries, noOld: null historicalEntries, bfs: backfillStatus }
              backfillCaveat =
                case bfState of
                  { bfs: BackfillFinished, noNew: true, noOld: true } -> Just "No FOAM Lite events yet – go relay some messages!"
                  { bfs: BackfillRunning, noNew: true, noOld: true } -> Just "No FOAM Lite events seen – searching chain history..."
                  { bfs: BackfillNeverStarted, noNew: true, noOld: true } -> Just "No FOAM Lite events seen yet"
                  { bfs: BackfillErrored e , noNew: true, noOld: true } -> Just $ "No FOAM Lite events seen yet -- searching chain history failed: " <> e
                  { bfs: BackfillRunning, noNew: _, noOld: _ } -> Just "Searching chain history for more FOAM Lite events..."
                  { bfs: BackfillErrored e, noNew: _, noOld: _ } -> Just $ "Searching chain history failed: " <> e
                  { bfs: _, noNew: _, noOld: _ } -> Nothing

                  
              ret str =
                HH.tr
                [ css "line-height-event-log border-dullergray border-opacity-75" ]
                [ HH.td
                  [ css "leading-loose pt-8 text-text_lightgray", fullWidthCell ]
                  [ HH.text str ]
                ]
           in case backfillCaveat of
                Nothing -> []
                Just bfc -> [ret bfc]

        tableEntry entry =
          let view = tableEntryView entry
              td elem = HH.td [ css "line-height-eventlog text-text_lightgray" ] [ elem ]
            in HH.tbody
               []
               [ HH.tr
                 [ css "hidden sm:table-row line-height-event-log border-b border-dullergray border-opacity-75 last:border-b-0" ]
                 [ td (HH.text $ view._type)
                 , td $ txHashLink view.txHash
                 , td $ justOrNA (addressLink <$> view.minter)
                 , td $ justOrNA (addressLink <$> view.owner)
                 , td $ tokenImage 16 view.tokenID ""
                 ]
               , HH.tr
                 [ css "sm:hidden border-dullergray border-opacity-75 last:border-b-0" ]
                 [ eventCard view ]
               ]

        eventCardMint view = case view.minter of
          Just minter -> Just
            [ HH.span [ css "w-full inline-block" ] [ HH.text "For ", addressLink minter ]
            ]
          Nothing -> Nothing
        eventCardTransfer view = case (Tuple view.owner view.destination) of 
          Tuple (Just owner) (Just destination) -> Just $
            [ HH.span [ css "w-full inline-block" ] [ HH.text "From ", addressLink owner ]
            , HH.span [ css "w-full inline-block" ] [ HH.text "To ", addressLink destination ]
            ]
          _ -> Nothing

        eventCard view =
          HH.td
          [ css "text-text_lightgray", fullWidthCell ]
          [ HH.div
            [ css "flex flex-no-wrap flex-root border rounded p-4 mt-10 h-40" ]
            [ HH.div
              [ css "w-1/3 h-full flex justify-center justify-items-center place-items-center border-r" ]
              [ HH.div [ ] [tokenImage 24 view.tokenID "mr-3" ] ]
            , HH.div
              [ css "w-2/3 h-full flex justify-center justify-items-center place-items-center text-left px-4" ]
              [ HH.div [ ]
                ( [ HH.span [ css "font-semibold w-full inline-block" ] [ HH.text $ view._type <> " #" <> show view.tokenID ] ] <>
                  ( (fromMaybe [] (eventCardMint view <|> eventCardTransfer view)) <>
                  [ HH.span [ css "w-full inline-block" ] [ HH.text "Relayed by ", addressLink view.relayer ]
                  , HH.span [ css "w-full inline-block" ] [ HH.text "Tx ", txHashLink view.txHash ]
                  ])
                )
              ]
            ]
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
            H.modify_ (\st -> st { entries = (entry : st.entries)})
            pure $ Just next
          InsertHistoricalTableEntry entry next -> do
            H.modify_ (\st -> st { historicalEntries = (entry : st.historicalEntries)})
            pure $ Just next
          BackfillStatusUpdated bfs next -> do
            H.modify_ (\st -> st { backfillStatus = bfs })
            pure $ Just next
          BlockExplorerUpdated newBlockExplorer next -> do
            H.modify_ (\st -> st { blockExplorer = newBlockExplorer } )
            pure $ Just next