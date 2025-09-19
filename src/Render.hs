{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Render where

import JS
import qualified Data.Text.Lazy as TL
import Lucid
import Data.Functor.Identity (Identity)
import qualified Codec.Picture as JP
import qualified Graphics.Rasterific as RA
import qualified Graphics.Rasterific.Texture as RA
import Data.Colour.Names (blue, white, black)
import Data.Colour (Colour)
import Data.Colour.SRGB (toSRGB, channelRed, channelGreen, channelBlue)
import qualified Data.ByteString.Lazy as BL
import Graphics.Text.TrueType (Font)

import qualified Data.List as List
import Data.Bool
import Lens.Micro.Platform ((^.))
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Foldable as F

import qualified GHC.Debug.Profile as GDP
import GHC.Debug.Profile.Types
import Data.Semigroup

import qualified GHC.Debug.Types.Closures as Debug
import IOTree
import Lib as GD
import Model
import Data.ByteUnits
import qualified Numeric


data ArrWordsLine k = CountLine k Int Int | FieldLine ClosureDetails

data ThunkLine = ThunkLine (Maybe SourceInformation) Count

data ProfileLine  = ProfileLine GDP.ProfileKey GDP.ProfileKeyArgs CensusStats | ClosureLine ClosureDetails


{- Page renderers -}
renderSocketSelectionPage :: SetupKind -> [SocketInfo] -> TL.Text
renderSocketSelectionPage st sockets = 
  renderText $ do
    h1_ $ toHtml ("Mode: " <> show st)
    form_ [method_ "post", action_ "/toggle-set-up"] $ do
      button_ "Toggle mode"
    h1_ $ toHtml ("Select a debuggee to connect to (found " <> pack (show (length sockets)) <> "):")
    if null sockets
      then mempty
      else form_ [method_ "post", action_ "/connect"] $ do
             ul_ $ F.forM_ (zip [0 :: Int .. ] sockets) $ \ (i, socket) ->
               li_ $ do
                 input_ $ [type_ "radio", name_ "socket", value_ (socketName socket)] ++ [checked_ | i == 0]
                 toHtml $ socketName socket <> " - " <> renderSocketTime socket
             button_ "Connect"

    h3_ $ "Upload eventlog"
    li_ $ "Compile with the -prof flag, or use cabal run --enable-profiling"
    li_ $ "Run the executable with the -l or -l-agu to produce a .eventlog file"
    li_ $ "Once the program has terminated, upload the eventlog to see analysis"
    li_ $ "Note: .hp files are also accepted, but produce less detailed output"
    form_ [method_ "post", enctype_ "multipart/form-data", action_ "/eventlogAnalysis"] $ do
      div_ [ style_ "display: flex; gap: 2rem; align-items: flex-start; margin-bottom: 0.25rem;" ] $ do
        -- Left column: Upload file
        div_ [ style_ "flex: 1; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;" ] $ do
          input_ [type_ "file", name_ "eventlog"]
          button_ [type_ "submit"] "Upload"
        
        -- Right column: Analysis buttons
        div_ [ style_ "flex: 1;" ] $ do
          li_ $ do
            input_ [type_ "radio", name_ "isJson", value_ "False", checked_]
            toHtml ("See on web" :: String)
          li_ $ do
            input_ [type_ "radio", name_ "isJson", value_ "True"]
            toHtml ("Create json" :: String)
          
          div_ [style_ "margin: 0; display: flex; align-items: center; gap: 8px;"] $ do
            p_ "Sort by: "
            select_ [ name_ "eventlog-sort" ] $ do
              option_ [value_ "size"] (toHtml ("Size" :: Text))
              option_ [value_ "stddev"] (toHtml ("Standard deviation" :: Text))
              option_ [value_ "name"] (toHtml ("Name" :: Text))
              option_ [value_ "number"] (toHtml ("Number" :: Text))
              option_ [value_ "gradient"] (toHtml ("Gradient" :: Text))
          label_ [for_ "elog-reverse"] " Reverse band order: "
          input_ [type_ "checkbox", id_ "elog-reverse", name_ "eLogRev"]

renderAlreadyConnectedPage :: TL.Text
renderAlreadyConnectedPage = renderText $ do
  h1_ "You are already connected!"
  form_ [method_ "get", action_ "/connect"] $ button_ "See debuggee"

renderBadSocketPage :: TL.Text
renderBadSocketPage = renderText $ do
  h1_ "Error: No debuggee this socket"
  form_ [method_ "get", action_ "/"] $ button_ "Select another socket"

renderConnectedPage :: [Int] -> CDIO -> SocketInfo -> Debuggee -> ConnectedMode -> TL.Text
renderConnectedPage selectedPath cdio socket _ mode' = renderText $ case mode' of
  RunningMode -> do
    h2_ "Status: running mode. There is nothing you can do until you pause the process."
    form_ [method_ "post", action_ "/pause"] $ button_ "Pause process" 
  PausedMode os -> pageLayout "/connect" $ do
    let tree = _treeSavedAndGCRoots os

    h2_ $ toHtml ("ghc-debug - Paused " <> socketName socket)
    form_ [method_ "post", action_ "/resume"] $ button_ "Resume process"
    form_ [method_ "post", action_ "/exit"] $ button_ "Exit"
  
    div_ [ style_ "display: flex; gap: 2rem; align-items: flex-start;" ] $ do
      div_ [ style_ "flex: 1; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;" ] $ do
        h3_ "Selection: "
        detailedSummary renderClosureSummary tree selectedPath cdio
      div_ [ style_ "flex: 1; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;" ] $ do
        renderMImg cdio

    form_ [ id_ "snapshotForm", style_ "margin: 0; display: flex; align-items: center; gap: 8px;"] $ do
      input_ [type_ "text", id_ "filenameInput", placeholder_ "Enter snapshot name", required_ "required"]
      button_ [type_ "submit"] "Take snapshot"
      div_ [id_ "snapshotResult"] ""
      takeSnapshotScript

    form_ [ id_ "searchLimitForm", style_ "margin: 0; display: flex; align-items: center; gap: 8px;" ] $ do
      input_ [type_ "number", id_ "searchLimitInput", placeholder_ "Enter search limit", required_ "required"]
      button_ [type_ "submit"] "Limit searches"
      toolTipSpan $ toHtmlRaw ("Default = 100<br>Enter 0 for unlimited searches" :: Text)
      div_ [id_ "limitResult"] ""
      setSearchLimitScript

    h3_ $ toHtml $ case os ^. treeMode of
      SavedAndGCRoots {} -> pack "Root Closures"
      Retainer {} -> pack "Retainers"
      SearchedHtml {} -> pack "Search Results"
    
    --table_ [style_ "border-collapse: collapse; width: 100%;"] $ do
    renderIOTreeHtml tree selectedPath (detailedRowHtml renderClosureHtml "connect") encodePath
    expandToggleScript
    selectTreeLinkScript

renderProfilePage :: (Show name, Ord name) => Utils a -> IOTree a name -> String
                  -> [Int] -> CDIO -> TL.Text
renderProfilePage Utils{..} tree name selectedPath cdio = renderText $ pageLayout "/profile" $ do
  h1_ "Profile"
  div_ $ a_ [href_ "/download-profile", download_ "profile_dump", style_ "display: inline-block; margin-top: 1em;" ] "Download"
  genericTreeBody tree selectedPath _renderRow _renderSummary name cdio

renderCountPage :: (Show name, Ord name) => String -> Utils a -> IOTree a name -> String
                -> [Int] -> CDIO -> TL.Text
renderCountPage title Utils{..} tree name selectedPath cdio =
  renderText $ pageLayout (T.pack $ "/" ++ name) $ do
    h1_ $ toHtml $ title ++ " Count"
    genericTreeBody tree selectedPath _renderRow _renderSummary name cdio
        
renderThunkAnalysisPage :: (Show name, Ord name) => Utils a -> IOTree a name -> String
                        -> [Int] -> CDIO -> TL.Text
renderThunkAnalysisPage Utils{..} tree name selectedPath cdio =
  renderText $ pageLayout "/thunkAnalysis" $ do
    h1_ "Thunk analysis"
    genericTreeBody tree selectedPath _renderRow _renderSummary name cdio
 

renderFilterSearchPage :: (Show name, Ord name) => IOTree ClosureDetails name -> Suggestions -> [UIFilter] 
                       -> Version -> [Int] -> CDIO -> TL.Text
renderFilterSearchPage tree Suggestions{..} filters' dbgVersion selectedPath cdio = 
  renderText $ pageLayout "/searchWithFilters" $ do
    h1_ "Results for search with filters"
    renderMImg cdio
    div_ [ style_ "display: flex; gap: 2rem; align-items: flex-start;" ] $ do
      -- Left column: Line summary
      div_ [ style_ "flex: 1; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;" ] $ do
        h3_ "Selection: "
        ul_ $ detailedSummary renderClosureSummary tree selectedPath cdio
    
      -- Middle column: List of filters
      div_ [ style_ "flex: 1;" ] $ do
        h3_ "Filters: "
        ul_ $ mapM_ (uncurry renderUIFilterHtml) (zip filters' [0..])
 
      -- Right column: Buttons for modifying filters
      div_ [ style_ "flex: 1;" ] $ do
        genFilterButtons "Enter closure address" "Address" 
        genFilterButtons "Enter info table address" "InfoAddress" 
        genFilterButtonsWithS _cons "Select constructor name" "ConstrName"
        genFilterButtonsWithS _cloNames "Select closure name" "ClosureName"
        genFilterButtons "Enter closure size (B)" "ClosureSize"
        genFilterButtonsWithS _cloTypes "Select closure type" "ClosureType"
        genFilterButtonsNoExclude "Enter ARR_WORDS size (B)" "ARR_WORDSSize"
        if inEraMode dbgVersion then genFilterButtons "Enter era" "Era" else mempty
        if inSomeProfMode dbgVersion then genFilterButtonsWithS _ccIds "Select cost centre id" "CCID"
                                     else mempty
        form_ [ method_ "post", action_ "/clearFilters"
              , style_ "margin: 0; display: flex; align-items: center; gap: 8px;"] $ do
          button_ [type_ "submit"] "Clear all filters"    
     
 
    renderIOTreeHtml tree selectedPath (detailedRowHtml renderClosureHtml "searchWithFilters") encodePath
    expandToggleScript
    selectTreeLinkScript


{- Summary renderers -}
renderClosureSummary :: ClosureDetails -> [Int] -> CDIO -> Html ()
renderClosureSummary node' path CDIO{..} =
  case node' of
    ClosureDetails _ excSize' info' -> do 
      renderInfoSummary info'
      li_ $ do
        strong_ "Exclusive size: "
        toHtml (show (getSize excSize') <> "B")
        case node' of 
          ClosureDetails{_closure = Closure{_closureSized = Debug.unDCS -> Debug.ArrWordsClosure{}}} -> do
            li_ $ a_ [href_ ("/dumpArrWords?selected=" <> encodePath path)] "Dump ARR_WORDS payload"
          _ -> mempty
        case _mInc of
          Nothing -> mempty
          Just (incSize, capped) -> renderIncSize incSize capped path
    LabelNode n -> li_ $ toHtml n
    InfoDetails info' -> renderInfoSummary info'
    CCSDetails _ _ptr (Debug.CCSPayload{..}) -> do
      li_ $ do 
        strong_ "ID: "
        toHtml (show ccsID) 
      renderCC ccsCc
    CCDetails _ c -> renderCC c

detailedSummary :: (Ord name, Show name)
                => (a -> [Int] -> CDIO -> Html ())
                -> IOTree a name -> [Int] -> CDIO -> Html ()
detailedSummary f tree path cdio =
  div_ [id_ "selection-summary"] $ do
    case getSubTree tree path of
      Nothing -> mempty
      Just (IOTreeNode node' _) -> f node' path cdio
 
summaryEntry :: (Monad m, Term (HtmlT m ()) result, ToHtml a) => HtmlT m () -> a -> result
summaryEntry title value = li_ $ strong_ (title <> ": ") >> toHtml value

renderProfileSummary :: CensusStats -> ProfileLine -> [Int] -> CDIO -> Html ()
renderProfileSummary totalStats line path cdio = do
  div_ [ style_ "display: flex; gap: 2rem; align-items: flex-start;" ] $ do
    -- Left column: Line summary
    div_ [ style_ "flex: 1; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;" ] $ do
      h3_ "Selection: "
      ul_ $ renderLineSummary line

    div_ [ style_ "flex: 1; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;" ] $ do
      renderMImg cdio 

    -- Right column: Total stats
    div_ [ style_ "flex: 1;" ] $ do
      h3_ "Total: "
      ul_ $ renderLineSummary (ProfileLine (GDP.ProfileClosureDesc "Total") GDP.NoArgs totalStats)
  where 
    renderLineSummary (ClosureLine cs) = renderClosureSummary cs path cdio
    renderLineSummary (ProfileLine k args (CS (Count n) (Size s) (Data.Semigroup.Max (Size mn)) _)) = do
      summaryEntry "Label" (truncT (GDP.prettyShortProfileKey k <> GDP.prettyShortProfileKeyArgs args))
      case k of
        GDP.ProfileConstrDesc desc -> do
          summaryEntry "Package" (GDP.pkgsText desc)
          summaryEntry "Module" (GDP.modlText desc)
          summaryEntry "Constructor" (GDP.nameText desc)
        _ -> mempty
      summaryEntry "Count" (show n)
      summaryEntry "Size" (renderBytesHtml s)
      summaryEntry "Max" (renderBytesHtml mn)
      summaryEntry "Average" (renderBytesHtml @Double (fromIntegral s / fromIntegral n))

renderCountSummary :: Show a => Maybe (Html ()) -> ArrWordsLine a -> [Int] -> CDIO -> Html ()
renderCountSummary mh line path cdio = do
  div_ [ style_ "display: flex; gap: 2rem; align-items: flex-start;" ] $ do
    -- Left column: Line summary
    div_ [ style_ "flex: 1; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;" ] $ do
      h3_ "Selection: "
      ul_ $ renderLineSummary line

    div_ [ style_ "flex: 1; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;" ] $ do
      renderMImg cdio

    -- Right column: Histogram
    case mh of
      Just histo -> 
        div_ [ style_ "flex: 1;" ] $ do
          h3_ "Histogram: "
          ul_ $ histo
          img_ [src_ "/histogram", alt_ "Histogram", style_ "max-width: 100%; height: auto; margin-top: 1rem; border: 1px solid #000000;" ]
      Nothing -> mempty

    where
      renderLineSummary :: Show a => ArrWordsLine a -> Html ()
      renderLineSummary (CountLine b l n) = do
        summaryEntry "Count" (show n)
        summaryEntry "Size" (renderBytesHtml l)
        summaryEntry "Total size" (renderBytesHtml $ n * l)
        li_ $ toHtml $ trunc (show b)
      renderLineSummary (FieldLine c) = renderClosureSummary c path cdio

renderThunkAnalysisSummary :: ThunkLine -> [Int] -> CDIO -> Html ()
renderThunkAnalysisSummary (ThunkLine msc c) _ _ = do
  h3_ "Selection: "
  case msc of
    Nothing -> toHtml ("NoLoc" :: Text)
    Just sc -> renderSourceInfoSummary sc
  summaryEntry "Count" (show $ getCount c)

{- Summary renderer helpers -}
prettyCCS :: GenCCSPayload CCSPtr CCPayload -> Text
prettyCCS Debug.CCSPayload{ccsCc = cc} = prettyCC cc

prettyCC :: CCPayload -> Text
prettyCC Debug.CCPayload{..} =
  T.pack ccLabel <> "   " <> T.pack ccMod <> "   " <> T.pack ccLoc

renderSourceInfoSummary :: SourceInformation -> Html ()
renderSourceInfoSummary (SourceInformation name cty ty label' modu loc) = do 
  summaryEntry "Name" name
  summaryEntry "Closure type" (show cty)
  summaryEntry "Type" ty
  summaryEntry "Label" label'
  summaryEntry "Module" modu
  summaryEntry "Location" loc


renderInfoSummary :: InfoInfo -> Html ()
renderInfoSummary info' = do 
  maybe mempty renderSourceInfoSummary (_sourceLocation info')
  case _profHeaderInfo info' of
    Just x ->
      let label' = case x of 
                     Debug.RetainerHeader{} -> "Retainer info"
                     Debug.LDVWord{} -> "LDV info"
                     Debug.EraWord{} -> "Era"
                     Debug.OtherHeader{} -> "Other"
      in summaryEntry label' (renderProfHeader x)
    Nothing -> mempty
  where renderProfHeader pinfo@(Debug.RetainerHeader {}) = show pinfo
        renderProfHeader (Debug.LDVWord {state, creationTime, lastUseTime}) = 
          (if state then "✓" else "✘") ++ 
          " created: " ++ 
          show creationTime ++ 
          (if state then " last used: " <> show lastUseTime else "")
        renderProfHeader (Debug.EraWord era) = show era
        renderProfHeader (Debug.OtherHeader other) = "Not supported: " ++ show other

renderCC :: CCPayload -> Html ()
renderCC Debug.CCPayload{..} = do
  summaryEntry "Label" ccLabel
  summaryEntry "CC ID" (show ccID)
  summaryEntry "Module" ccMod
  summaryEntry "Location" ccLoc
  summaryEntry "Allocation" (show ccMemAlloc)
  summaryEntry "Time ticks" (show ccTimeTicks)
  summaryEntry "Is CAF" (show ccIsCaf)



{- Tree renderers -}
genericTreeBody :: (Ord name, Show name) => IOTree node name -> [Int] -> (node -> Html ())
                -> (node -> [Int] -> CDIO -> Html ()) -> String -> CDIO
                -> HtmlT Identity ()
genericTreeBody tree selectedPath renderRow renderSummary' name cdio = do
  detailedSummary renderSummary' tree selectedPath cdio
  h3_ "Results"
  renderIOTreeHtml tree selectedPath (detailedRowHtml renderRow name) encodePath
  expandToggleScript
  selectTreeLinkScript

detailedRowHtml :: (a -> Html ()) -> String -> [Int] -> Bool -> Bool -> a -> Html ()
detailedRowHtml renderHtml name thisPath expanded selected obj =
  let depth = length thisPath
      indentPx = depth * 20
      classStr = "tree-row" <> if selected then " selected" else ""
      styleAttr = style_ $ pack ("margin-left: " <> show indentPx <> "px; display: flex; align-items: center; gap: 4px;")
      pathStr = encodePath thisPath
      linkStyle = "color: " ++ (if selected then "purple" else "blue") ++ "; text-decoration: none;"
      dataPathAttr = data_ "path" pathStr
  in div_ [class_ classStr, styleAttr] $ do
       form_ [style_ "margin: 0;", class_ "expand-form", dataPathAttr] $ do
         button_ 
           [ type_ "button"
           , class_ "expand-button"
           , data_ "expanded" (pack $ if expanded then "true" else "false")
           , dataPathAttr
           ]
           (toHtml $ if expanded then "+" else "-" :: String)
       a_ [href_ ("/" <> pack name <> "?selected=" <> pathStr), class_ "tree-link", style_ (pack $ linkStyle), data_ "selected" pathStr] $ renderHtml obj

renderClosureHtml :: ClosureDetails -> Html ()
renderClosureHtml (ClosureDetails closure' _excSize info') = div_ [class_ "closure-row"] $ do
  li_ $ toHtml $ _labelInParent info' <> " | " <> pack (closureShowAddress closure') <> " | " <> _pretty info' 
  {-table_ [style_ "border-collapse: collapse; width: 100%;"] $ do
    tr_ [style_ "background-color: #f5f5f5; cursor: pointer;"] $ do
      td_ [style_ "padding: 12px; border: 1px solid #ccc; text-align: left;"] $ toHtml $ _labelInParent info'
      td_ [style_ "padding: 12px; border: 1px solid #ccc; text-align: left;"] $ toHtml $ pack (closureShowAddress closure')
      td_ [style_ "padding: 12px; border: 1px solid #ccc; text-align: left;"] $ toHtml $ _pretty info'
-}
renderClosureHtml (InfoDetails info') = div_ [class_ "closure-row"] $ do
  li_ $ toHtml $ _labelInParent info' <> " | " <> _pretty info' 
renderClosureHtml (LabelNode t) = div_ [class_ "closure-row"] $ do
  li_ $ toHtml $ t
renderClosureHtml (CCSDetails clabel _cptr ccspayload) = div_ [class_ "closure-row"] $ do
  li_ $ toHtml $ clabel <> " | " <> prettyCCS ccspayload 
renderClosureHtml (CCDetails clabel cc) = div_ [class_ "closure-row"] $ do
  li_ $ toHtml $ clabel <> " | " <> prettyCC cc

renderProfileHtml :: ProfileLine -> Html ()
renderProfileHtml (ClosureLine c) = renderClosureHtml c 
renderProfileHtml (ProfileLine k kargs c) = div_ [class_ "profile-line-row"] $ do
  li_ $ toHtml $ GDP.prettyShortProfileKey k <> GDP.prettyShortProfileKeyArgs kargs <> " " <> showLine c
  where showLine (CS (Count n) (Size s) (Data.Semigroup.Max (Size mn)) _) =
          pack (show s ++ " " ++ show n ++ " " ++ show mn ++ " " ++
                (Numeric.showFFloat @Double (Just 1) (fromIntegral s / fromIntegral n) ""))

renderCountHtml :: Show a => ArrWordsLine a -> Html ()
renderCountHtml (CountLine k l n) = div_ [class_ "arr-count-row"] $ do
  li_ $ toHtml $ show n <> " " <> renderBytesHtml l <> " " <> (take 100 $ show k)
renderCountHtml (FieldLine cd) = renderClosureHtml cd

renderThunkAnalysisHtml :: ThunkLine -> Html ()
renderThunkAnalysisHtml (ThunkLine msc (Count c)) =
  li_ $ toHtml $ header <> " " <> show c
  where header = case msc of
                   Just sc -> infoPosition sc
                   Nothing -> "NoLoc"

{- Tree renderer helpers -}
closureFormat :: ClosureDetails -> String
closureFormat (ClosureDetails _ _ inf) = List.intercalate "\n" $ 
  [ payload ]
  where payload = if isList body then listify body
                    else if savedObj label' && (head body /= '_' && '#' `notElem` body)
                      then takeWhile (/=' ') body
                      else trunc body
        savedObj s = (s == "Saved Object" || List.isPrefixOf "Field" s || s == "Indirectee")
        label' = T.unpack (_labelInParent inf)
        body = T.unpack (_pretty inf)
        isList xs = ':' `elem` xs
        listify xs = case words xs of [_, ":", _] -> "(:)"; _ -> xs
closureFormat (InfoDetails inf) = T.unpack (_labelInParent inf)
closureFormat (LabelNode l) = T.unpack l
closureFormat (CCSDetails clabel _ _) = T.unpack clabel
closureFormat (CCDetails clabel _) = T.unpack clabel
ccsFormat :: GenCCSPayload ccsPtr CCPayload -> [Char]
ccsFormat Debug.CCSPayload{ccsCc = cc} = ccFormat cc
ccFormat :: CCPayload -> [Char]
ccFormat Debug.CCPayload{..} = List.intercalate "\n" $
  [ "Label: " ++ ccLabel, "Module: " ++ ccMod, "Location: " ++ ccLoc]

closureName :: ClosureDetails -> String
closureName (ClosureDetails c _ _) = closureShowAddress c 
closureName (InfoDetails inf) = T.unpack (_labelInParent inf)
closureName (LabelNode l) = T.unpack l
closureName (CCSDetails clabel _ _) = T.unpack $ clabel
closureName (CCDetails clabel _) = T.unpack $ clabel

closureGetSize :: ClosureDetails -> Int
closureGetSize x = case x of ClosureDetails _ excSize' _ -> getSize excSize'; _ -> 0
countGetName :: ArrWordsLine a -> Maybe String
countGetName x = case x of FieldLine c -> Just (closureName c); _ -> Nothing
countFormat :: ArrWordsLine a -> String
countFormat x = case x of FieldLine c -> closureFormat c; _ -> ""
countGetSize :: ArrWordsLine a -> Int
countGetSize x = case x of FieldLine (ClosureDetails _ excSize' _) -> getSize excSize'; _ -> 0


{- General page layout/navigation -}
data Tab = Tab { tabName :: Text, tabRoute :: Text }

tabs :: [Tab]
tabs = 
  [ Tab "Home" "/connect"
  , Tab "Profile" "/profile"
  , Tab "ARR_WORDS Count" "/arrWordsCount"
  , Tab "Strings Count" "/stringsCount"
  , Tab "Thunk Analysis" "/thunkAnalysis"
  , Tab "Search retainers" "/searchWithFilters"
  ]

pageLayout :: Text -> Html () -> Html ()
pageLayout currentRoute bodyContent = do
  head_ $ do
    title_ "Debuggee"
    style_ navStyle
  body_ $ do
    navBar currentRoute
    div_ [class_ "content"] bodyContent

navBar :: Text -> Html ()
navBar current = nav_ [class_ "navbar"] $
  ul_ $ F.forM_ tabs $ \ tab -> do
    let active = if tabRoute tab == current then "active" else ""
    li_ [class_ (T.unwords ["tab", active])] $
      if tabRoute tab == "/profile"
        then form_ [method_ "post", action_ "/profile", style_ "display:inline"] $ do
               select_ [name_ "profileLevel", onchange_ "this.form.submit()"] $ do
                 option_ [value_ "1"] (toHtml ("Level one" :: Text))
                 option_ [value_ "2"] (toHtml ("Level two" :: Text))
               button_ [type_ "submit", class_ "tab-button"] "Profile"
        else form_ [method_ "post", action_ (tabRoute tab), style_ "display:inline"] $
               button_ [type_ "submit", class_ "tab-button"] (toHtml (tabName tab))

navStyle :: Text
navStyle = T.unlines
  [ ".navbar { background: #f0f0f0; padding: 10px; }"
  , ".tab { display: inline; margin-right: 20px; }"
  , ".tab-button { background: none; border: none; color: black; font-weight: bold; cursor: pointer; }"
  , ".active .tab-button { color: blue; } "
  , ".tab-button:focus { outline: none; }"
  , ".content { margin-top: 20px }"
  ]


{- Image renderers -}
renderMImg :: CDIO -> Html ()
renderMImg CDIO {..} = do 
  if not _hasGV 
    then do
      h3_ "You don't have Graphviz installed, which is required to display visualisations of closures" 
      p_ "Run sudo apt install graphviz"
    else case _imgInfo of
      Nothing -> renderEmptyImgPanel
      Just ImgInfo{..} -> renderImgPage _name _capped
  script_ [src_ "https://cdn.jsdelivr.net/npm/panzoom@9.4.0/dist/panzoom.min.js"] (mempty :: Html ())

renderEmptyImgPanel :: Html ()
renderEmptyImgPanel = do
  div_ [style_ "margin: 0; display: flex; align-items: center; gap: 8px;"] $ do
    h3_ [id_ "image-title"] "Visualisation"
    button_ [id_ "toggleButton", onclick_ "toggleDiv()", disabled_ "true"] "Show" 
  div_ [id_ "toggleDiv", style_ "display: none;", data_ "available" "false"] mempty

renderImgPage :: String -> Bool -> Html () 
renderImgPage name capped = do
  div_ [ style_ "margin: 0; display: flex; align-items: center; gap: 8px;"] $ do
    h3_ [id_ "image-title"] $ toHtml $ "Visualisation of " ++ name
    button_ [ id_ "toggleButton", onclick_ "toggleDiv()" ] "Show"
    label_ [for_ "fastModeCheckbox"] " Fast mode: "
    input_ [type_ "checkbox", id_ "fastModeCheckbox", onchange_ "fastModeToggle()"]
    toolTipSpan $ toHtmlRaw ("Yellow = Root node<br>Green = click to see children<br>Light green = root node, click to see children" :: Text)
  p_ [id_ "capWarning"] (if capped then "Note: this is a very large object, and this graph is incomplete" else "")
  div_ [ id_ "toggleDiv", style_ "display: none;" ] $ do 
    body_ $ do
      div_ $ a_ [ href_ "/graph.svg"
                , id_ "download-link"
                , download_ "graph.svg"
                , style_ "display: none; margin-top: 1em;"
                ] "Download SVG"
      div_ [id_ "svg-container", style_ "border: 1px solid #ccc; width: 100%; max-width: 800px; height: 80vh; overflow: hidden;"] $ 
        mempty -- the actual svg is loaded via JS on demand (see selectTreeLink.js)

{- Filter renderers -}
genFilterButtons :: String -> String -> Html ()
genFilterButtons = genFilterButtons' Nothing True
genFilterButtonsNoExclude :: String -> String -> Html ()
genFilterButtonsNoExclude = genFilterButtons' Nothing False
genFilterButtonsWithS :: [String] -> String -> String -> Html ()
genFilterButtonsWithS suggs = genFilterButtons' (Just suggs) True
genFilterButtons' :: Maybe [String] -> Bool -> String -> String -> Html ()
genFilterButtons' suggs exclude flavourText filterType = do
  let selectId    = pack $ "select-" ++ filterType
      inputId     = pack $ "input-" ++ filterType
      containerId = pack $ "container-" ++ filterType

  form_ [ method_ "post", action_ "/addFilter"
        , style_ "margin: 0; display: flex; align-items: flex-start; gap: 8px;"
        , id_ containerId ] $ do

    -- Container for select + optional input
    div_ [ style_ "display: flex; flex-direction: column; gap: 4px;" ] $ do
      let lenStyle = style_ "width: 24ch; margin: 0;"
      case suggs of
        Just suggs' -> do
          select_ [name_ "pattern", id_ selectId, onchange_ (pack $
                  "handleSelectChange('" ++ T.unpack selectId ++ "', '" ++ T.unpack inputId ++ "')"),
                  required_ "required", lenStyle] $ do
            option_ [disabled_ "disabled", selected_ "selected", hidden_ "hidden"] (toHtml flavourText)
            F.forM_ suggs' (\s -> option_ [value_ (pack s)] (toHtml s))
            option_ [value_ "__other__"] "Other..."

          -- Hidden text input underneath
          input_ [ type_ "text", name_ "pattern", id_ inputId
                 , placeholder_ "Enter custom value"
                 , style_ "display: none;" ]

        Nothing -> input_ [ type_ "text", name_ "pattern", placeholder_ (pack flavourText)
                          , required_ "required", lenStyle ]

    input_ [type_ "hidden", name_ "filterType", value_ (pack filterType)]
    button_ [type_ "submit", name_ "invert", value_ "False"] "Add filter"
    if exclude then button_ [type_ "submit", name_ "invert", value_ "True"] "Exclude" else mempty
    -- JS to toggle input visibility
    otherTextInputScript

renderUIFilterHtml :: UIFilter -> Int -> Html ()
renderUIFilterHtml (UIAddressFilter inv x) = renderUIFLine inv "Closure address" show x 
renderUIFilterHtml (UIInfoAddressFilter inv x) = renderUIFLine inv "Info table address" show x
renderUIFilterHtml (UIConstructorFilter inv x) = renderUIFLine inv "Constructor name" id x
renderUIFilterHtml (UIInfoNameFilter inv x) = renderUIFLine inv "Constructor name (exact)" id x
renderUIFilterHtml (UIEraFilter inv x) = renderUIFLine inv "Era range" showEraRange x
renderUIFilterHtml (UISizeFilter inv x) = renderUIFLine inv "Size (lower bound)" (show . getSize) x
renderUIFilterHtml (UIClosureTypeFilter inv x) = renderUIFLine inv "Closure type" show x
renderUIFilterHtml (UICcId inv x) = renderUIFLine inv "CC Id" show x
renderUIFLine :: Bool -> String -> (a -> String) -> a -> Int -> Html ()
renderUIFLine inv desc show' x (-1) = 
  li_ $ toHtml $ (bool "" "!" inv) <> desc <> ": " <> show' x 
renderUIFLine inv desc show' x i = 
  div_ [class_ "filter-line", style_ "display: flex; align-items: center; gap: 8px;"] $ do
    form_ [method_ "post", action_ "/deleteFilter", value_ "undefined", style_ "margin: 0;"] $ do
      input_ [type_ "hidden", name_ "index", value_ (pack $ show i)]
      button_ [type_ "submit", class_ "delete-button"] "x"
    div_ [] $ toHtml $ (bool "" "!" inv) <> desc <> ": " <> show' x 
plainUIFilters :: [UIFilter] -> Html ()
plainUIFilters fs = mapM_ (uncurry renderUIFilterHtml) (zip fs (repeat (-1)))

{- Histogram renderers -}
histogramHtml :: Int -> [GD.Size] -> Html ()
histogramHtml boxes m = do
  mapM_ displayLine (bin 0 (map calcPercentage (List.sort m )))
  where
    Size maxSize = maximum m
    calcPercentage (Size tot) = (tot, (fromIntegral tot/ fromIntegral maxSize) * 100 :: Double)
    displayLine (l, h, n, tot) =
      li_ $ toHtml $ show l <> "%-" <> show h <> "% (" <> show n <> " objects, total size: " <> renderBytesHtml tot ++ ")"
    step = fromIntegral (ceiling @Double @Int (100 / fromIntegral boxes))
    bin _ [] = []
    bin k xs =
      let upper = min (k + step) 100
          (now, later) = span (\(_, p) -> p <= upper) xs
      in case now of
           [] -> bin (k + step) later
           _  -> (round k :: Int, round upper :: Int, length now, sum (map fst now)) : bin (k + step) later

renderHistogramImage :: Font -> Int -> [GD.Size] -> BL.ByteString
renderHistogramImage font boxes sizes = JP.encodePng image
  where
    maxSize = maximum [s | GD.Size s <- sizes]
    unSize (GD.Size s) = s
    sortedSizes = List.sort (map unSize sizes)
    
    step = ceiling (fromIntegral maxSize / fromIntegral boxes :: Double) :: Int
    bin _ [] = []
    bin k xs =
      let (now, later) = span (<= k + step) xs
      in case now of
          [] -> bin (k + step) later
          _  -> (k, k + step, length now, sum now) : bin (k + step) later
    bins = bin 0 sortedSizes

    width = 800
    height = 400
    barWidth = (width - 80) `div` boxes
    leftPadding = 50.0 :: Float
    bottomPadding = 30.0 :: Float
    topPadding = 10.0 :: Float
    usableHeight = fromIntegral height - bottomPadding - topPadding

    maxCount = maximum (0 : [count | (_, _, count, _) <- bins])

    scaleY n = (fromIntegral n / fromIntegral maxCount) * usableHeight

    image :: JP.Image JP.PixelRGBA8
    image = RA.renderDrawing width height (toRGBA8 white) $ do
      let axisColor = toRGBA8 black
          fontSize :: RA.PointSize
          fontSize = RA.PointSize 10

      -- Y-axis
      RA.withTexture (RA.uniformTexture axisColor) $
        RA.stroke 2.0 RA.JoinRound (RA.CapRound, RA.CapRound) $
          RA.line (RA.V2 leftPadding topPadding) (RA.V2 leftPadding (fromIntegral height - bottomPadding))

      -- X-axis
      RA.withTexture (RA.uniformTexture axisColor) $
        RA.stroke 2.0 RA.JoinRound (RA.CapRound, RA.CapRound) $
          RA.line (RA.V2 leftPadding (fromIntegral height - bottomPadding)) (RA.V2 (fromIntegral width - 10) (fromIntegral height - bottomPadding))

      -- Bars
      F.forM_ (zip [0..] bins) $ \(i, (lo, hi, count, _)) -> do
        let x = leftPadding + fromIntegral (i * barWidth)
            h = scaleY count
            y = fromIntegral height - bottomPadding - h

        -- Bar
        RA.withTexture (RA.uniformTexture (toRGBA8 blue)) $
          RA.fill $ RA.rectangle (RA.V2 x y) (fromIntegral barWidth - 2) h

        -- X-axis label
        let label' = show lo ++ "–" ++ show hi ++ "B"
            labelX = x + 2
            yOffset = 10
            labelY = fromIntegral height - bottomPadding + 4 + yOffset
        RA.withTexture (RA.uniformTexture axisColor) $
          RA.printTextAt font fontSize (RA.V2 labelX labelY) label'

        let xAxisTitle = "Object Size"
            xAxisTitleSize = RA.PointSize 12
            xAxisTitleX = leftPadding + (fromIntegral width - leftPadding) / 2 - 40  -- center-ish
            xAxisTitleY = fromIntegral height - 5  -- slightly below x-axis
        RA.withTexture (RA.uniformTexture axisColor) $
          RA.printTextAt font xAxisTitleSize (RA.V2 xAxisTitleX xAxisTitleY) xAxisTitle

      -- Y-axis ticks and labels
      let yTicks = 5 :: Int
      F.forM_ ([0..yTicks] :: [Int]) $ \ (i :: Int) -> do
        let val = round (fromIntegral maxCount * fromIntegral i / fromIntegral yTicks :: Double) :: Int
            yOffset = 10
            y = fromIntegral height - bottomPadding - (fromIntegral i * usableHeight / fromIntegral yTicks)
            label' = show val
        -- Tick mark
        RA.withTexture (RA.uniformTexture axisColor) $
          RA.stroke 1.0 RA.JoinRound (RA.CapRound, RA.CapRound) $
            RA.line (RA.V2 (leftPadding - 5) y) (RA.V2 leftPadding y)
        -- Label
        RA.withTexture (RA.uniformTexture axisColor) $
          RA.printTextAt font fontSize (RA.V2 (leftPadding - 35) (y - 5 + yOffset)) label'

      let yAxisTitle = "Freq"
          yAxisTitleSize = RA.PointSize 12
          yAxisTitleX = 5
          yAxisTitleY = topPadding + (usableHeight / 2)
      RA.withTexture (RA.uniformTexture axisColor) $
        RA.printTextAt font yAxisTitleSize (RA.V2 yAxisTitleX yAxisTitleY) yAxisTitle

toRGBA8 :: Colour Double -> JP.PixelRGBA8
toRGBA8 c = 
  let srgb = toSRGB c  -- Gives RGB Double in [0,1]
      r = floor (channelRed srgb * 255) :: Integer 
      g = floor (channelGreen srgb * 255) :: Integer
      b = floor (channelBlue srgb * 255) :: Integer
  in JP.PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255



{- Additional renderers -}

-- {- Message param must be HTML, i.e. for newlines, use <br> -}
toolTipSpan :: Html () -> Html ()
toolTipSpan msg =
  -- Tooltip container span
  span_ 
    [ style_ "display: inline-block; position: relative; margin-left: 8px; cursor: pointer; font-weight: bold; border: 1px solid #ccc; border-radius: 50%; width: 18px; height: 18px; text-align: center; line-height: 18px; background-color: #eee;"
    , onmouseover_ "this.querySelector('span').style.visibility='visible'; this.querySelector('span').style.opacity='1';"
    , onmouseout_ "this.querySelector('span').style.visibility='hidden'; this.querySelector('span').style.opacity='0';"
    ] $ do
      "?"
      -- Tooltip text span
      span_ 
        [ style_ "visibility: hidden; width: 160px; background-color: black; color: white; text-align: center; border-radius: 6px; padding: 5px 0; position: absolute; z-index: 1; bottom: 125%; left: 50%; margin-left: -80px; opacity: 0; transition: opacity 0.3s;"
        ] $ msg

-- {- Renders inclusive size of closures, with warning message if incomplete -}
renderIncSize :: Int -> Bool -> [Int] -> Html ()
renderIncSize incSize capped _ = do 
  li_ $ do
    strong_ "Inclusive size: "
    toHtml ((if capped then ">=" else "") ++
            show incSize ++ "B" ++ 
            (if capped then " (Note: this is a very large object. This is a partial calculation)" else ""))
  -- Possible button here for 'unsafe' full calculation
  {-if capped then 
    form_ [method_ "post", action_ "/incSize"] $ do
      input_ [type_ "hidden", name_ "selected", value_ (encodePath selectedPath)]
      button_ [type_ "submit", class_ "inc-button"] $ "See inclusive size"
  else mempty-}

renderBytesHtml :: Real a => a -> String
renderBytesHtml n = getShortHand (getAppropriateUnits (ByteValue (realToFrac n) Bytes))
