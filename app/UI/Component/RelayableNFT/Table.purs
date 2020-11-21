module UI.Component.RelayableNFT.Table where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader (class MonadAsk)
import Data.Array (length, null, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HHC
import Halogen.HTML.Properties as HP
import Network.Ethereum.Core.BigNumber (BigNumber, embed)
import Prim.Row as Row
import UI.Component.Icons as Icons
import UI.Component.RelayableNFT.Types (TableEntry, TableEntryView, tableEntryView)
import UI.Component.TokenIcon.Component as TokenIcon
import UI.Config (BlockExplorer, blockExplorerAddressLink, blockExplorerTxLink)
import UI.Monad (AppEnv)
import UI.Utils (css)

foreign import ellipsize :: Int -> Int -> String -> String

_nftTable :: SProxy "nftTable"
_nftTable = SProxy

data BackfillStatus = 
    BackfillNeverStarted
  | BackfillRunning BigNumber
  | BackfillFinished
  | BackfillErrored String

type State = { entries :: Array TableEntry
             , historicalEntries :: Array TableEntry
             , blockExplorer :: Maybe BlockExplorer
             , backfillStatus :: BackfillStatus
             , backfillExtents :: Maybe BackfillExtents
             }

type BackfillExtents = { start :: BigNumber, end :: BigNumber }

data Query a
  = InsertNewTableEntry TableEntry a
  | InsertHistoricalTableEntry TableEntry a
  | BackfillExtentsDetermined BackfillExtents a
  | BackfillStatusUpdated BackfillStatus a
  | BlockExplorerUpdated (Maybe BlockExplorer) a

data Action

type Input = Unit

type Message = Void

type Slots = ()

component
  :: ∀ m.
     MonadAff m
  => MonadAsk AppEnv m
  => H.Component HH.HTML Query Input Message m
component =
  H.mkComponent
    { initialState: const $ { entries: [] {- generateTableEntry <$> (1..50) -}
                            , historicalEntries: []
                            , backfillStatus: BackfillNeverStarted
                            , backfillExtents: Nothing
                            , blockExplorer: Nothing
                            }
    , render
    , eval
    }
  where

    render :: State -> H.ComponentHTML Action Slots m
    render { entries, historicalEntries, backfillStatus, backfillExtents, blockExplorer } = 
      HH.div
        [ css "flex w-full px-8 md:p-8 lg:pt-20 text-center" ]
        [
          HH.table
          [ css "w-full" ]
          ([ tableHead ] <> renderTableEntries)
        ]
      where
        tableHeadings = ["Tx Hash", "Event", "Relayer", "Token"]
        tableHead = 
          HH.thead
          []
          [ HH.tr
            [ css "hidden sm:table-row line-height-event-log border-b border-white" ]
            ((\th -> HH.th [css $ "font-medium line-height-event-log " <> if th /= "Token" then "text-left" else "text-center"] [ HH.text th ]) <$> tableHeadings)
          ]

        responsiveHashString' loose str =
          HH.div [ css "contents" ]
          [ HH.span [ css "inline md:hidden" ] [HH.text $ ellipsize (if loose then 3 else 2) (if loose then 3 else 2) str]
          , HH.span [ css "hidden md:inline xl:hidden" ] [HH.text $ ellipsize (if loose then 3 else 2) (if loose then 3 else 2) str]
          , HH.span [ css "hidden xl:inline xxl:hidden" ] [HH.text $ ellipsize (if loose then 4 else 2) (if loose then 4 else 2) str]
          , HH.span [ css "hidden xxl:inline 3xl:hidden" ] [HH.text $ "0x" <> ellipsize (if loose then 10 else 5) (if loose then 10 else 5) str]
          , HH.span [ css "hidden 3xl:inline 4xl:hidden" ] [HH.text $ "0x" <> ellipsize (if loose then 14 else 7) (if loose then 14 else 7) str]
          , HH.span [ css "hidden 4xl:inline" ] [HH.text $ "0x" <> ellipsize (if loose then 20 else 10) (if loose then 20 else 10) str]
          ]
        responsiveHashString = responsiveHashString' false

        txHashLink' loose txHash = responsiveLink' loose (show txHash) $ blockExplorerTxLink blockExplorer txHash
        txHashLink = txHashLink' false
        addressLink' loose addr = responsiveLink' loose (show addr) $ blockExplorerAddressLink blockExplorer addr
        addressLink = addressLink' false
              
        responsiveLink' loose text href = case href of
          Nothing -> HH.span [ css "inline-block" ] [ responsiveHashString' loose text ]
          Just href' ->
              HH.a
              [ HP.href href', HP.target "_blank", css "underline" ]
              [ responsiveHashString' loose text, Icons.externalLink 4 ["inline-block", "-mb-1"] [] ]
        responsiveLink = responsiveLink' false

        fullWidthCell = HP.colSpan (length tableHeadings)

        tokenImage size view extras =
          HH.img
            [ HP.alt tokenIDAsName
            , HP.title tokenIDAsName
            , HP.src ("/static-token-images/" <> show (view.tokenID `mod` (embed 3) + (embed 1)) <> ".png")
            , css ("w-" <> show size <> " h-" <> show size <> " inline-block" <> " " <> extras)
            ]
          where tokenIDAsName = "Token #" <> show view.tokenID
          -- let classes = ("w-"<>sz<>" h-"<>sz<>" inline-block" <> if extras /= "" then " " <> extras else "")
          --     sz = show size
          --     containerProps = [ css classes ]
          --     iconProps = {}
          --  in TokenIcon.componentHTML containerProps iconProps


        justOrNA = fromMaybe (HH.text "N/A")

        renderTableEntries = renderNewTableEntries <> historicalEntriesInfo <> renderHistoricalTableEntries

        renderNewTableEntries = tableEntry <$> entries
        renderHistoricalTableEntries = tableEntry <$> historicalEntries
        ellipsisOrBackfillPercentage bn = case backfillExtents of
          {-
          Just { start, end } ->
            let totalBlocks = end - start
                percent = (bn - start) / totalBlocks
                percentStr = show percent
             in "... (" <> if percentStr == "100" then "99" else percentStr  <> "%)" -- never show 100 cause otherwise we'd be at BackfillFinished :P
          -}
          _ -> "..."
        historicalEntriesInfo =
          let bfState@{noNew, noOld, bfs} = { noNew: null entries, noOld: null historicalEntries, bfs: backfillStatus }
              backfillCaveat =
                case bfState of
                  { bfs: BackfillFinished, noNew: true, noOld: true } -> Just "No FOAM Lite events yet – go relay some messages!"
                  { bfs: BackfillRunning bn, noNew: true, noOld: true } -> Just $ "No FOAM Lite events seen – searching chain history" <> ellipsisOrBackfillPercentage bn
                  { bfs: BackfillNeverStarted, noNew: true, noOld: true } -> Just "No FOAM Lite events seen yet -- waiting to connect to Ethereum..."
                  { bfs: BackfillErrored e , noNew: true, noOld: true } -> Just $ "No FOAM Lite events seen yet -- searching chain history failed: " <> e
                  { bfs: BackfillRunning bn, noNew: _, noOld: _ } -> Just $ "Searching chain history for more FOAM Lite events" <> ellipsisOrBackfillPercentage bn
                  { bfs: BackfillErrored _, noNew: _, noOld: true } -> Just $ "Failed to fetch any historical FOAM Lite events"
                  { bfs: BackfillErrored _, noNew: _, noOld: false } -> Just $ "Coudn't fetch all historical FOAM Lite events"
                  { bfs: _, noNew: _, noOld: _ } -> Nothing

              caveatHasBorder = not null historicalEntries
              ret str =
                HH.tr
                [ css $ "line-height-event-log border-dullergray border-opacity-75" <> (if caveatHasBorder then " border-b" else "") ]
                [ HH.td
                  [ css $ "leading-loose pt-8 text-text_lightgray" <> (if caveatHasBorder then " pb-8" else ""), fullWidthCell ]
                  [ HH.text str ]
                ]
           in case backfillCaveat of
                Nothing -> []
                Just bfc -> [ret bfc]

        tableEntry entry =
          let view = tableEntryView entry
              td = td' ""
              td' cls = HH.td [ css $ "line-height-eventlog text-text_lightgray" <> (if cls == "" then "" else " " <> cls) ]
              descriptionCell = td' "text-left" $ case view of
                { owner: Just o, destination: Just d } -> [ HH.text $ view._type <> " #" <> show view.tokenID <> " from ", addressLink o, HH.text " to ", addressLink d ]
                { minter: Just m } -> [ HH.span_ [HH.text $ view._type <> " #" <> show view.tokenID <> " for "], addressLink m ]
                _ -> [ HH.text "???" ]
                
            in HH.tbody
               []
               [ HH.tr
                 [ css "hidden sm:table-row line-height-event-log border-b border-dullergray border-opacity-75 last:border-b-0" ]
                 [ td' "text-left" [ txHashLink view.txHash ]
                 , descriptionCell
                 , td' "text-left" [ addressLink view.relayer ]
                 , td [ tokenImage 16 view {- "pt-4.5" -} "" ]
                 ]
               , HH.tr
                 [ css "sm:hidden border-dullergray border-opacity-75 last:border-b-0" ]
                 [ eventCard view ]
               ]

        eventCardMint view = case view.minter of
          Just minter -> Just
            [ HH.span [ css "w-full inline-block" ] [ HH.text "For ", addressLink' true minter ]
            ]
          Nothing -> Nothing
        eventCardTransfer view = case (Tuple view.owner view.destination) of 
          Tuple (Just owner) (Just destination) -> Just $
            [ HH.span [ css "w-full inline-block" ] [ HH.text "From ", addressLink' true owner ]
            , HH.span [ css "w-full inline-block" ] [ HH.text "To ", addressLink' true destination ]
            ]
          _ -> Nothing

        eventCard view =
          HH.td
          [ css "text-text_lightgray", fullWidthCell ]
          [ HH.div
            [ css "flex flex-no-wrap flex-root border rounded p-4 mt-10 h-56" ]
            [ HH.div
              [ css "w-1/3 h-full flex justify-center justify-items-center place-items-center border-r" ]
              [ tokenImage 24 view "mr-3" ]
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
         ~> H.HalogenM State Action Slots Message m
    eval = H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , initialize = Nothing
      }
      where
        handleQuery :: forall a.  Query a -> H.HalogenM State Action Slots Message m (Maybe a)
        handleQuery = case _ of
          InsertNewTableEntry entry next -> do
            H.modify_ (\st -> st { entries = (entry : st.entries)})
            pure $ Just next
          InsertHistoricalTableEntry entry next -> do
            H.modify_ (\st -> st { historicalEntries = (entry : st.historicalEntries)})
            pure $ Just next
          BackfillExtentsDetermined extents next -> do
            H.modify_ (\st -> st { backfillExtents = Just extents })
            pure $ Just next
          BackfillStatusUpdated bfs next -> do
            H.modify_ (\st -> st { backfillStatus = bfs })
            pure $ Just next
          BlockExplorerUpdated newBlockExplorer next -> do
            H.modify_ (\st -> st { blockExplorer = newBlockExplorer } )
            pure $ Just next