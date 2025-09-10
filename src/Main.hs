{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Web.Scotty as Scotty
import Data.IORef
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Lucid
import qualified Control.Exception as E
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.GraphViz (GraphID(Str), toLabel, runGraphviz, GraphvizOutput(Svg), isGraphvizInstalled, runGraphvizCommand, GraphvizCommand(..), printDotGraph)
import Data.GraphViz.Attributes.Complete (Attribute(URL, Height, Margin, Width, Shape, NodeSep, RankSep, Sep, Overlap, K), DPoint(DVal), Shape(Circle), Overlap(ScaleXYOverlaps))
import Data.GraphViz.Types.Monadic (node, edge, digraph)
import Data.GraphViz.Types.Generalised (DotGraph, graphStatements, GlobalAttributes(GraphAttrs, NodeAttrs), DotStatement(GA))
import Web.Scotty.Internal.Types
import Data.Functor.Identity (Identity)
import Data.String (IsString)
import Debug.Trace
import qualified Codec.Picture as JP
import qualified Graphics.Rasterific as RA
import qualified Graphics.Rasterific.Texture as RA
import Data.Colour.Names (blue, white, black)
import Data.Colour (Colour)
import Data.Colour.SRGB (toSRGB, channelRed, channelGreen, channelBlue)
import qualified Data.ByteString.Lazy as BL
import Graphics.Text.TrueType (loadFontFile, Font)
import Network.Wai.Parse (FileInfo(..))
import Eventlog.Data (generateJsonValidate, HeapProfileData(..), eventlogHeapProfile, generateJson)
import Eventlog.HtmlTemplate (templateString)
import qualified Eventlog.Args as EA
import Data.Aeson (encode, object, (.=))
import GHC.Debug.Client.Query (dereferenceConDesc, dereferenceCCS, dereferenceCC, getSourceInfo)
import Data.Maybe (catMaybes)
import qualified Network.Wai.Middleware.Static as NWMS
import Control.Concurrent.Async
import Network.HTTP.Types (status409, status500)
import System.Process
import System.IO

import Control.Applicative
import Control.Monad (forM)
import Control.Monad.IO.Class
import qualified Data.List as List
import Data.Ord (comparing)
import qualified Data.Ord as Ord
import Lens.Micro.Platform ((^.), (&), (.~), (%~))
import System.Directory
import System.FilePath
import Data.Bool
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as S
import qualified Data.Foldable as F
import Text.Read (readMaybe)

import qualified GHC.Debug.Profile as GDP
import GHC.Debug.Profile.Types
import Data.Semigroup

import GHC.Debug.Types.Ptr(readInfoTablePtr, arrWordsBS)
import qualified GHC.Debug.Types.Closures as Debug
import IOTree
import Lib as GD
import Model
import Data.ByteUnits
import qualified Numeric

-- STATUS: Done (in use)
updateListFrom :: MonadIO m =>
                        IO FilePath
                        -> m [SocketInfo]
updateListFrom dirIO = liftIO $ do
            dir :: FilePath <- dirIO
            debuggeeSocketFiles :: [FilePath] <- listDirectory dir <|> return []
            -- Sort the sockets by the time they have been created, newest
            -- first.
            debuggeeSockets <- List.sortBy (comparing Ord.Down)
                                  <$> mapM (mkSocketInfo . (dir </>)) debuggeeSocketFiles
            
            return $ debuggeeSockets

-- STATUS: Done (in use superficially, might not actually ever be used)
getChildren :: Debuggee -> ClosureDetails
            -> IO [ClosureDetails]
getChildren _ LabelNode{} = return []
getChildren _ CCDetails {} = return []
getChildren _ InfoDetails {} = return []
getChildren d (ClosureDetails c _ _) = do
  children <- closureReferences d c
  children' <- traverse (traverse (fillListItem d)) children
  mapM (\(lbl, child) -> getClosureDetails d (pack lbl) child) children'
getChildren d (CCSDetails _ _ cp) = do
  references <- zip [0 :: Int ..] <$> ccsReferences d cp
  mapM (\(lbl, cc) -> getClosureDetails d (pack (show lbl)) cc) references

-- STATUS: Done (in use)
fillListItem :: Debuggee
             -> ListItem CCSPtr SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr
             -> IO (ListItem CCSPtr SrtCont PayloadCont ConstrDesc StackCont ClosurePtr)
fillListItem _ (ListOnlyInfo x) = return $ ListOnlyInfo x
fillListItem d (ListFullClosure cd) = ListFullClosure <$> fillConstrDesc d cd
fillListItem _ ListData = return ListData
fillListItem _ (ListCCS c1 c2) = return $ ListCCS c1 c2
fillListItem _ (ListCC c1) = return $ ListCC c1

-- STATUS: Done (in use)
mkIOTree :: Debuggee
         -> [a]
         -> (Debuggee -> a -> IO [a])
         -> ([a] -> [a])
         -> IOTree a Name
mkIOTree debuggee' cs getChildrenGen sort = ioTree Connected_Paused_ClosureTree
        (sort cs)
        (\c -> sort <$> getChildrenGen debuggee' c)

-- STATUS: Done (in use)
prettyCCS :: GenCCSPayload CCSPtr CCPayload -> Text
prettyCCS Debug.CCSPayload{ccsCc = cc} = prettyCC cc

-- STATUS: Done (in use)
prettyCC :: CCPayload -> Text
prettyCC Debug.CCPayload{..} =
  T.pack ccLabel <> "   " <> T.pack ccMod <> "   " <> T.pack ccLoc

-- STATUS: Done (in use)
completeClosureDetails :: Debuggee -> (Text, DebugClosure CCSPtr SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr)
                                            -> IO ClosureDetails

completeClosureDetails dbg (label', clos)  =
  getClosureDetails dbg label' . ListFullClosure  =<< fillConstrDesc dbg clos


-- STATUS: Done (in use)
getClosureDetails :: Debuggee
                            -> Text
                            -> ListItem CCSPtr SrtCont PayloadCont ConstrDesc StackCont ClosurePtr
                            -> IO ClosureDetails
getClosureDetails debuggee' t (ListOnlyInfo info_ptr) = do
  info' <- getInfoInfo debuggee' t info_ptr
  return $ InfoDetails info'
getClosureDetails _ plabel (ListCCS ccs payload) = return $ CCSDetails plabel ccs payload
getClosureDetails _ plabel (ListCC cc) = return $ CCDetails plabel cc
getClosureDetails _ t ListData = return $ LabelNode t
getClosureDetails debuggee' label' (ListFullClosure c) = do
  let excSize' = closureExclusiveSize c
  sourceLoc <- maybe (return Nothing) (infoSourceLocation debuggee') (closureInfoPtr c)
  pretty' <- closurePretty debuggee' c
  return ClosureDetails
    { _closure = c
    , _info = InfoInfo {
       _pretty = pack pretty'
      , _labelInParent = label'
      , _sourceLocation = sourceLoc
      , _closureType = Just (T.pack $ show c)
      , _constructor = Nothing
      , _profHeaderInfo  = case c of
          Closure{_closureSized=c1} -> Debug.hp <$> Debug.profHeader (Debug.unDCS c1)
          _ -> Nothing
      }
    , _excSize = excSize'
    }

-- STATUS: Done (in use)
getInfoInfo :: Debuggee -> Text -> InfoTablePtr -> IO InfoInfo
getInfoInfo debuggee' label' infoPtr = do

  sourceLoc <- infoSourceLocation debuggee' infoPtr
  let pretty' = case sourceLoc of
                  Just loc -> pack (infoPosition loc)
                  Nothing -> ""
  return $ InfoInfo {
       _pretty = pretty'
      , _labelInParent = label'
      , _sourceLocation = sourceLoc
      , _closureType = Nothing
      , _constructor = Nothing
      , _profHeaderInfo = Nothing
      }

-- STATUS: Done (in use)
mkRetainerTree :: Debuggee -> [[ClosureDetails]] -> IOTree ClosureDetails Name
mkRetainerTree dbg stacks = do
  let stack_map = [ (cp, rest) | stack <- stacks, Just (cp, rest) <- [List.uncons stack]]
      roots = map fst stack_map
      info_map :: M.Map Ptr [(Text, (DebugClosure CCSPtr SrtCont PayloadCont ConstrDesc StackCont ClosurePtr))]
      info_map = M.fromList [(toPtr (_closure k), zipWith (\n cp -> ((T.pack (show n)), (_closure cp))) [0 :: Int ..] v) | (k, v) <- stack_map]

      lookup_c dbg' dc'@(ClosureDetails dc _ _) = do
        let ptr = toPtr dc
            results = M.findWithDefault [] ptr info_map
        -- We are also looking up the children of the object we are retaining,
        -- and displaying them prior to the retainer stack
        cs <- getChildren dbg' dc'
        results' <- liftIO $ mapM (\(l, c) -> getClosureDetails dbg' l (ListFullClosure c)) results
        return (cs ++ results')
      -- And if it's not a closure, just do the normal thing
      lookup_c dbg' dc' = getChildren dbg' dc'

  mkIOTree dbg roots lookup_c id


data ArrWordsLine k = CountLine k Int Int | FieldLine ClosureDetails

data ThunkLine = ThunkLine (Maybe SourceInformation) Count

data ProfileLine  = ProfileLine GDP.ProfileKey GDP.ProfileKeyArgs CensusStats | ClosureLine ClosureDetails


{- New code here -}

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

renderAlreadyConnectedPage :: TL.Text
renderAlreadyConnectedPage =
  renderText $ do
    h1_ "You are already connected!"
    form_ [method_ "get", action_ "/connect"] $ do
      button_ "See debuggee"

renderBadSocketPage :: TL.Text
renderBadSocketPage =
  renderText $ do
    h1_ "Error: No debuggee this socket"
    form_ [method_ "get", action_ "/"] $ do
      button_ "Select another socket"

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
    title_ "BLAH"
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

renderConnectedPage :: [Int] -> CDIO -> SocketInfo -> Debuggee -> ConnectedMode -> TL.Text
renderConnectedPage selectedPath cdio socket _ mode' = renderText $ case mode' of
  RunningMode -> do
    h2_ "Status: running mode. There is nothing you can do until you pause the process."
    form_ [method_ "post", action_ "/pause"] $ 
      button_ "Pause process" 
  PausedMode os -> pageLayout "/connect" $ do
    let tree = _treeSavedAndGCRoots os

    h2_ $ toHtml ("ghc-debug - Paused " <> socketName socket)
    form_ [method_ "post", action_ "/resume"] $
      button_ "Resume process"
    form_ [method_ "post", action_ "/exit"] $
      button_ "Exit"

    
  
    div_ [ style_ "display: flex; gap: 2rem; align-items: flex-start;" ] $ do
      div_ [ style_ "flex: 1; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;" ] $ do
        h3_ "Selection: "
        detailedSummary renderClosureSummary tree selectedPath cdio
      div_ [ style_ "flex: 1; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;" ] $ do
        renderMImg cdio

    form_ [ method_ "post", action_ "/takeSnapshot"
          , style_ "margin: 0; display: flex; align-items: center; gap: 8px;"] $ do
      input_ [type_ "text", name_ "filename", placeholder_ "Enter snapshot name", required_ "required"]
      input_ [type_ "hidden", name_ "selected", value_ (encodePath selectedPath)]
      button_ [type_ "submit"] "Take snapshot"

    form_ [ method_ "post", action_ "/setSearchLimit"
          , style_ "margin: 0; display: flex; align-items: center; gap: 8px;"] $ do
      input_ [type_ "text", name_ "index", placeholder_ "Enter search limit", required_ "required"]

      input_ [type_ "hidden", name_ "selected", value_ (encodePath selectedPath)]
      button_ [type_ "submit"] "Limit searches"

    h3_ $ toHtml $ case os ^. treeMode of
      SavedAndGCRoots {} -> pack "Root Closures"
      Retainer {} -> pack "Retainers"
      SearchedHtml {} -> pack "Search Results"
    
    --table_ [style_ "border-collapse: collapse; width: 100%;"] $ do
    renderIOTreeHtml tree selectedPath (detailedRowHtml renderClosureHtml "connect") encodePath
    expandToggleScript
    selectTreeLinkScript
   
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
  if capped then p_ $ "Note: this is a very large object, and this tree is incomplete" else mempty
  div_ [ id_ "toggleDiv", style_ "display: none;" ] $ do 
    body_ $ do
      div_ $ a_ [ href_ "/graph.svg"
                , id_ "download-link"
                , download_ "graph.svg"
                , style_ "display: none; margin-top: 1em;"
                ] "Download SVG"
      -- this code renders the svg with JS so it is interactive
      div_ [id_ "svg-container", style_ "border: 1px solid #ccc; width: 100%; max-width: 800px; height: 80vh; overflow: hidden;"] $ 
        mempty

genericTreeBody :: (Ord name, Show name) => IOTree node name -> [Int] -> (node -> Html ())
                -> (node -> [Int] -> CDIO -> Html ()) -> String -> CDIO
                -> HtmlT Identity ()
genericTreeBody tree selectedPath renderRow renderSummary' name cdio = do
  detailedSummary renderSummary' tree selectedPath cdio
  h3_ "Results"
  renderIOTreeHtml tree selectedPath (detailedRowHtml renderRow name) encodePath
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
renderCountPage title Utils{..} tree name selectedPath cdio = renderText $ pageLayout (T.pack $ "/" ++ name) $ do
  h1_ $ toHtml $ title ++ " Count"
  genericTreeBody tree selectedPath _renderRow _renderSummary name cdio
        
renderThunkAnalysisPage :: (Show name, Ord name) => Utils a -> IOTree a name -> String
                        -> [Int] -> CDIO -> TL.Text
renderThunkAnalysisPage Utils{..} tree name selectedPath cdio = renderText $ pageLayout "/thunkAnalysis" $ do
  h1_ "Thunk analysis"
  genericTreeBody tree selectedPath _renderRow _renderSummary name cdio
  

renderFilterSearchPage :: (Show name, Ord name) => IOTree ClosureDetails name -> Suggestions -> [UIFilter] 
                       -> Version -> [Int] -> CDIO -> TL.Text
renderFilterSearchPage tree Suggestions{..} filters' dbgVersion selectedPath cdio = renderText $ pageLayout "/searchWithFilters" $ do
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
            F.forM_ suggs' $ \s ->
              option_ [value_ (pack s)] (toHtml s)
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
    script_ $ pack $
      "function handleSelectChange(selectId, inputId) {\n\
      \  var select = document.getElementById(selectId);\n\
      \  var input = document.getElementById(inputId);\n\
      \  if (select && input) {\n\
      \    if (select.value === '__other__') {\n\
      \      input.style.display = 'inline-block';\n\
      \      input.required = true;\n\
      \      select.name = '';\n\
      \      input.name = 'pattern';\n\
      \    } else {\n\
      \      input.style.display = 'none';\n\
      \      input.required = false;\n\
      \      input.name = '';\n\
      \      select.name = 'pattern';\n\
      \    }\n\
      \  }\n\
      \}"

        
detailedRowHtml :: (a -> Html ()) -> String -> [Int] -> [Int] -> Bool -> Bool -> a -> Html ()
detailedRowHtml renderHtml name selectedPath thisPath expanded selected obj =
  let depth = length thisPath
      indentPx = depth * 20
      classStr = "tree-row" <> if selected then " selected" else ""
      styleAttr = style_ $ pack ("margin-left: " <> show indentPx <> "px; display: flex; align-items: center; gap: 4px;")
      pathStr = encodePath thisPath
      selectedStr = encodePath selectedPath
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

getClosureIncSize :: (a -> Maybe String) -> (a -> Int) -> Set.Set String -> IOTreeNode a name -> Int
getClosureIncSize getName' getSize' seen' node' = fst (go seen' node')
  where
    go seen (IOTreeNode n csE) =
      case getName' n of
        Nothing -> case csE of 
                     Left _ -> (0, seen)
                     Right cs -> listApply go 0 seen cs
        Just ptr -> 
          if Set.member ptr seen
          then (0, seen)
          else case csE of 
                 Left _ -> (getSize' n, seen) 
                 Right cs -> let (cSize, newSeen) = listApply go 0 (Set.insert ptr seen) cs
                             in ((getSize' n + cSize), newSeen)
    listApply f res s xs = foldl step (res, s) xs
      where step (acc, st) x = let (a, st') = f st x in (acc + a, st') 

type EdgeList = [(String, String, Int)]

getClosureVizTree :: (a -> String) -> (a -> String) -> Set.Set String -> [(String, String)] -> EdgeList -> IOTreeNode a name -> (Set.Set String, [(String, String)], EdgeList)
getClosureVizTree getName' format' nodes formattedNodes edges (IOTreeNode n csE) = 
  let ptr = getName' n
  in if Set.member ptr nodes
     then (nodes, formattedNodes, [])
     else case csE of
            Left _ -> (Set.insert ptr nodes, (ptr, format' n) : formattedNodes, [])
            Right cs -> 
              let nodes'' = Set.insert ptr nodes
                  fnodes'' = (ptr, format' n) : formattedNodes
                  (nodesFinal, fNodesFinal, childEdges) = listApply (getClosureVizTree getName' format') (nodes'', fnodes'', edges) cs
                  children = [ getName' n'
                             | IOTreeNode n' _ <- cs ]
                  newEdges = map (\(ch, i) -> (ptr, ch, i)) (zip children [0..])
              in (nodesFinal, fNodesFinal, childEdges ++ newEdges)
  where
    listApply f (ns, fns, es) xs =
      foldl (\(nsAcc, fnsAcc, esAcc) x ->
               let (ns', fns', es') = f nsAcc fnsAcc [] x
               in (ns', fns', esAcc ++ es')) (ns, fns, es) xs

renderBytesHtml :: Real a => a -> String
renderBytesHtml n = getShortHand (getAppropriateUnits (ByteValue (realToFrac n) Bytes))

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

closureFormat :: ClosureDetails -> String
closureFormat (ClosureDetails clo excSize' inf) = List.intercalate "\n" $ 
  [ payload
  , "Address: " ++ closureShowAddress clo
  , "Size: " ++ show (getSize excSize') ++ "B" ]
  where payload = if savedObj label' && (head body /= '_' && '#' `notElem` body)
                    then takeWhile (/=' ') body
                    else trunc body
        savedObj s = (s == "Saved Object" || List.isPrefixOf "Field" s || s == "Indirectee")
        label' = T.unpack (_labelInParent inf)
        body = T.unpack (_pretty inf)
closureFormat (InfoDetails inf) = T.unpack (_labelInParent inf)
closureFormat (LabelNode l) = T.unpack l
closureFormat (CCSDetails clabel _cptr ccspayload) = T.unpack clabel ++ "\n" ++ ccsFormat ccspayload
closureFormat (CCDetails clabel cc) = "Cost centre: " ++ T.unpack clabel ++ "\n" ++ ccFormat cc
ccsFormat :: GenCCSPayload ccsPtr CCPayload -> [Char]
ccsFormat Debug.CCSPayload{ccsCc = cc} = ccFormat cc
ccFormat :: CCPayload -> [Char]
ccFormat Debug.CCPayload{..} = List.intercalate "\n" $
  [ "Label: " ++ ccLabel, "Module: " ++ ccMod, "Location: " ++ ccLoc]

parsePath :: String -> [Int]
parsePath [] = []
parsePath s = map read $ splitOn "-" s

parseProfileLevel :: String -> ProfileLevel
parseProfileLevel "1" = OneLevel
parseProfileLevel "2" = TwoLevel  
parseProfileLevel _ = error "Error: profile level not supported"

truncN :: Int -> String -> String
truncN n s = take n s ++ (if length s > n then "..." else "")
trunc :: String -> String
trunc = truncN 30
truncT :: Text -> Text
truncT = pack . trunc . T.unpack

encodePath :: [Int] -> Text
encodePath = pack . List.intercalate "-" . map show

togglePath :: Eq a => [a] -> [[a]] -> [[a]]
togglePath x xs = if x `elem` xs then filter (/=x) xs else x:xs

toggleSelected :: Eq a => [a] -> [a] -> [a]
toggleSelected selectedPath togglePath'
  | sLen <= tLen = selectedPath
  | take tLen selectedPath == togglePath' = togglePath'
  | otherwise = selectedPath
  where sLen = length selectedPath
        tLen = length togglePath'

readParam :: t1 -> (t1 -> Scotty.ActionM String) -> (String -> t2) -> t2 -> Scotty.ActionM t2
readParam name getParam f def = do
  result <- (getParam name >>= return . f) `Scotty.catch` (\ (_ :: E.SomeException) -> return def)
  return result

type ParamGet t r = (t -> Scotty.ActionM String) -> Scotty.ActionM r
selectedParam :: Data.String.IsString t => ParamGet t [Int]
selectedParam getParam = readParam "selected" getParam parsePath [0]
togglePathParam :: Data.String.IsString t => ParamGet t [Int]
togglePathParam getParam = readParam "toggle" getParam parsePath []
profileLevelParam :: Data.String.IsString t => ParamGet t ProfileLevel
profileLevelParam getParam = readParam "profileLevel" getParam parseProfileLevel OneLevel 
filterTypeParam :: Data.String.IsString t => ParamGet t String
filterTypeParam getParam = readParam "filterType" getParam id ""
patternParam :: Data.String.IsString t => ParamGet t String
patternParam getParam = readParam "pattern" getParam id ""
invertParam :: Data.String.IsString t => ParamGet t Bool
invertParam getParam = readParam "invert" getParam read False
indexParam :: Data.String.IsString t => ParamGet t Int
indexParam getParam = readParam "index" getParam read (-1)
searchLimitParam :: Data.String.IsString t => ParamGet t (Maybe Int)
searchLimitParam getParam = readParam "index" getParam readMaybe Nothing
eLogOutParam :: Data.String.IsString t => ParamGet t Bool
eLogOutParam getParam = readParam "isJson" getParam (maybe False id . readMaybe) False
filterChangedParam :: Data.String.IsString t => ParamGet t Bool
filterChangedParam getParam = readParam "filterChanged" getParam (maybe False id . readMaybe) False
fastModeParam :: Data.String.IsString t => ParamGet t Bool
fastModeParam getParam = readParam "fastMode" getParam (maybe False id . readMaybe) False

buildClosureGraph :: [String] -> [(String, String)] -> EdgeList -> Data.GraphViz.Types.Generalised.DotGraph Int
buildClosureGraph nodes fnodes edges = digraph (Str "Visualisation") $ do
  -- possible style for source node, except this logic doesn't always select the source node
  -- nids@((sn, sid):rest)
  --node sid [toLabel (pack sn :: Text), Data.GraphViz.style filled, fillColor Yellow, color Red]
  mapM_ (\(n, nid) -> node nid [toLabel (pack (maybe "" id (lookup n fnodes)) :: Text)
                               {-, URL (TL.pack $ "https://localhost:3000/blah/" ++ show nid)-}]) nids
  mapM_ (\(a, b, eid) -> case (lookup a nids, lookup b nids) of
                      (Just x, Just y) -> edge x y [toLabel (pack (show eid) :: Text)]
                      z -> error ("Error in building closure graph: " ++ 
                                  "Arg a: " ++ a ++ ", Arg b: " ++ b ++ " -> " ++ (show z) ++ " -- nids : " ++ show nids)) edges
  where nids = zipWith (\n i -> (n,i)) nodes [1..]


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


expandToggleScript = script_ [src_ "expandToggle.js", defer_ ""] (mempty :: Html ())
selectTreeLinkScript = script_ [src_ "selectTreeLink.js", defer_ ""] (mempty :: Html ())

svgPath :: String
svgPath = "tmp/graph.svg"

genericGet :: IORef AppState
           -> Scotty.RoutePattern
           -> (forall a . Utils a -> IOTree a Name -> String -> [Int] -> CDIO -> TL.Text)
           -> Scotty.ScottyM ()
genericGet appStateRef index renderPage = do
  Scotty.get index $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected _ _ mode' ->
        case mode' of
          PausedMode os -> do 
            case _treeMode os of 
               SavedAndGCRoots{} -> Scotty.redirect "/connect"
               Retainer tree' suggs -> do
                 cdio <- getCDIO tree' selectedPath (Just . closureName) closureGetSize getNodeName closureFormat
                 updateImg appStateRef cdio
                 Scotty.html $ renderFilterSearchPage tree' suggs (_filters os) (_version os) selectedPath cdio
               SearchedHtml u@(Utils{..}) tree name -> do
                 let nameFn = maybe "" id . _getName 
                 cdio <- getCDIO tree selectedPath _getName _getSize
                         (\(IOTreeNode n' _) -> nameFn n') _graphFormat
                 
                 updateImg appStateRef cdio
                 Scotty.html $ renderPage u tree name selectedPath cdio
          RunningMode -> Scotty.redirect "/connect"
      Setup{} -> Scotty.redirect "/"

dumpArrWord :: ClosureDetails -> ActionT IO ()
dumpArrWord cs = do
  case cs of
    ClosureDetails{_closure = Closure{_closureSized = Debug.unDCS -> Debug.ArrWordsClosure{bytes, arrWords}}} -> do
      let payload = arrWordsBS (take (fromIntegral bytes) arrWords)
      Scotty.setHeader "Content-Type" "application/octet-stream"
      Scotty.setHeader "Content-Disposition" "attachment; filename=\"closure.bin\""
      Scotty.raw payload
    _ -> mempty
arrDumpProf :: ProfileLine -> ActionT IO () 
arrDumpProf (ClosureLine cs) = dumpArrWord cs 
arrDumpProf _ = mempty
arrDumpCount :: ArrWordsLine a -> ActionT IO () 
arrDumpCount (FieldLine cs) = dumpArrWord cs 
arrDumpCount _ = mempty
arrDumpThunk :: ThunkLine -> ActionT IO () 
arrDumpThunk _ = mempty

handleConnect :: Foldable t => IORef AppState -> AppState -> TL.LazyText -> t SocketInfo
                            -> (SocketInfo -> IO Bool)
                            -> ((Text -> IO ()) -> FilePath -> IO Debuggee)
                            -> Web.Scotty.Internal.Types.ActionT IO ()
handleConnect appStateRef state formValue options isValid' connect = do
  let match = F.find (\s -> TL.fromStrict (socketName s) == formValue) options
  case match of
    Just socketLike -> do
      valid <- liftIO $ isValid' socketLike
      if valid
        then do
          debuggee' <- liftIO $ connect (\_->return ()) (_socketLocation socketLike)
          let newState = state & majorState .~ Connected
                      { _debuggeeSocket = socketLike
                      , _debuggee = debuggee'
                      , _mode = RunningMode
                      }
          liftIO $ writeIORef appStateRef newState
          Scotty.redirect "/connect"
        else Scotty.html renderBadSocketPage
    Nothing -> Scotty.html renderBadSocketPage

getNodeName :: IOTreeNode ClosureDetails name -> String
getNodeName (IOTreeNode n _) = closureName n
closureName :: ClosureDetails -> String
closureName (ClosureDetails c _ _) = closureShowAddress c 
closureName (InfoDetails inf) = T.unpack (_labelInParent inf)
closureName (LabelNode l) = T.unpack l
closureName (CCSDetails clabel _ _) = T.unpack $ clabel
closureName (CCDetails clabel _) = T.unpack $ clabel

updateImg appStateRef (CDIO _ (Just ImgInfo{..}) _) = do
  state <- liftIO $ readIORef appStateRef
  case state ^. majorState of
    Connected s d (PausedMode os) -> do
      let newOs = os { _genSvg = _svgContent }
      let newState = state { _majorState = Connected s d (PausedMode newOs) } 
      liftIO $ writeIORef appStateRef newState
      return ()
    _ -> return ()
updateImg _ _ = return ()

getCDIO tree selectedPath getName' getSize' nodeName format' = 
  case getSubTree tree selectedPath of
    Nothing -> do
      hasGV <- liftIO $ isGraphvizInstalled
      return $ CDIO Nothing Nothing hasGV 
    Just subtree -> do
      (expSubTree, capped) <- liftIO $ expandNodeSafe subtree (maybe "" id . getName')
      let mInc = Just (getClosureIncSize getName' getSize' Set.empty expSubTree, capped)
      hasGV <- liftIO $ isGraphvizInstalled
      if not hasGV then return $ CDIO mInc Nothing hasGV
      else do
        imgInfo <- handleImg expSubTree capped nodeName getName' format'
        return $ CDIO mInc imgInfo hasGV

imgName :: CDIO -> String
imgName (CDIO _ (Just ImgInfo{..}) _) = _name
imgName _ = ""

graphvizProcess comm outPath dotGraph = do
  let cmd = case comm of
              Dot ->  "dot"
              _ -> "sfdp"
      args = ["-Tsvg", "-o", outPath]
  let processSpec = (proc cmd args)
        { std_in = CreatePipe
        , std_err = NoStream
        }
  withCreateProcess processSpec $ \(Just hIn) _ _ ph -> do
    hSetBuffering hIn LineBuffering
    hPutStrLn hIn (TL.unpack $ printDotGraph dotGraph)
    hClose hIn
    _ <- waitForProcess ph
    return ()

handleImg :: IOTreeNode a name -> Bool -> (IOTreeNode a name -> String) -> (a -> Maybe String) -> (a -> String) -> Scotty.ActionM (Maybe ImgInfo)
handleImg expSubtree@(IOTreeNode n' _) capped nodeName getName'' format' = do
  case getName'' n' of
    Just _ -> do 
      let getName' = maybe "" id . getName''
      let name = nodeName expSubtree
      let (nodes', fNodes, vizEdges) = getClosureVizTree getName' format' Set.empty [] [] expSubtree
      let vizNodes = Set.toList nodes'
      let graph = buildClosureGraph vizNodes fNodes vizEdges
      let tweakGraph :: GraphvizCommand -> DotGraph Int -> DotGraph Int
          tweakGraph Dot g = g
          tweakGraph comm g = g { graphStatements = (graphStatements g) <> stmts }
            where stmts = [ GA $ GraphAttrs [Overlap ScaleXYOverlaps, Sep (DVal 0.1), NodeSep 0.05, RankSep [0.05]] 
                          , GA $ NodeAttrs [Shape Circle, Width 0.05, Height 0.05, Margin (DVal 0.01) ]]
      let svgContent comm = liftIO $ do 
                              createDirectoryIfMissing True "tmp"
                              --_ <- runGraphvizCommand comm (tweakGraph comm graph) Svg svgPath
                              _ <- graphvizProcess comm svgPath (tweakGraph comm graph)
                              return ()
      return $ Just $ ImgInfo name capped svgContent
    Nothing -> return Nothing

closureGetName :: ClosureDetails -> Maybe String
closureGetName x = case x of ClosureDetails{} -> Just (closureName x); _ -> Nothing
closureGetSize :: ClosureDetails -> Int
closureGetSize x = case x of ClosureDetails _ excSize' _ -> getSize excSize'; _ -> 0
countGetName :: ArrWordsLine a -> Maybe String
countGetName x = case x of FieldLine c -> Just (closureName c); _ -> Nothing
countFormat :: ArrWordsLine a -> String
countFormat x = case x of FieldLine c -> closureFormat c; _ -> ""
countGetSize :: ArrWordsLine a -> Int
countGetSize x = case x of FieldLine (ClosureDetails _ excSize' _) -> getSize excSize'; _ -> 0
countTM :: Show a => Maybe (Html ()) -> IOTree (ArrWordsLine a) Name -> String
        -> (TreeMode, Utils (ArrWordsLine a))
countTM histo tree name = (SearchedHtml utils tree name, utils)
  where utils = Utils renderCountHtml (renderCountSummary histo) countFormat arrDumpCount countGetName countGetSize

updateAppState :: t -> (t -> OperationalState) -> SocketInfo -> Debuggee -> AppState -> AppState
updateAppState os f socket debuggee' state = newAppState
  where newOs = f os
        newMajorState = Connected socket debuggee' (PausedMode newOs)
        newAppState = state & majorState .~ newMajorState

setTM :: TreeMode -> OperationalState -> OperationalState
setTM treeMode' os = os { _treeMode = treeMode' }


getConstructors :: MonadIO m => Debuggee
                -> [[DebugClosure ccs srt pap ConstrDescCont s b]] -> m [String]
getConstructors debuggee' forSearch = do 
  let f clos = case clos of
                 Debug.ConstrClosure{} -> Just $ dereferenceConDesc (Debug.constrDesc clos)
                 _ -> Nothing
  let conDescs = map (map (f . Debug.unDCS . _closureSized)) forSearch
  conDescs' <- liftIO $ mapM (run debuggee') (concatMap catMaybes conDescs)
  --let cons = [ name x | x <- conDescs', pkg x `notElem` (["",{-"ghc-prim",-}"base"] :: [String]) ]
  return $ List.nub (map name conDescs')

getClosureNames :: MonadIO m => Debuggee
                -> [[DebugClosure ccs srt pap ConstrDescCont s b]] -> m [String]
getClosureNames debuggee' forSearch = do
  let f clos = Debug.tableId $ Debug.info $ Debug.unDCS $ _closureSized clos
  let cloNames = List.sort $ List.nub $ concat $ map (map f) forSearch
  cloNames' <- liftIO $ mapM (run debuggee') (map getSourceInfo cloNames)
  return (map infoName $ catMaybes cloNames')

getClosureTypes :: Monad m
                => [[DebugClosure ccs srt pap string s b]] -> m [String]
getClosureTypes forSearch = do
  let f clos = tipe $ Debug.decodedTable $ Debug.info $ Debug.unDCS $ _closureSized clos
  let cloTypes = List.sort $ List.nub $ concat $ map (map f) forSearch
  return (map show cloTypes)

getCcIds :: (Foldable t, MonadIO m)
         => Debuggee -> t [DebugClosure CCSPtr srt pap string s b] -> m [String]
getCcIds debuggee' forSearch = do
  let f clos = Debug.profHeader $ Debug.unDCS $ _closureSized clos
  let ccsIds = map (dereferenceCCS . Debug.ccs) $ catMaybes $ concatMap (map f) forSearch
  ccsIds' <- liftIO $ mapM (run debuggee') ccsIds
  let ccsIds'' = List.nub $ map Debug.ccsCc ccsIds'
  ccIds <- liftIO $ mapM (run debuggee') (map dereferenceCC ccsIds'')
  let ccIds' = List.sort $ List.nub $ map Debug.ccID ccIds
  return (map show ccIds')

--getSuggestions :: MonadIO m => GHC.Debug.Client.Monad.DebugM ClosureFilter 
--                            -> Debuggee -> m Suggestions
getSuggestions mClosFilter debuggee' = do 
  forSearch <- liftIO $ retainersOf Nothing mClosFilter Nothing debuggee'
  constrs <- getConstructors debuggee' forSearch
  cloNames <- getClosureNames debuggee' forSearch
  cloTypes <- getClosureTypes forSearch
  ccIds <- getCcIds debuggee' forSearch
  return $ Suggestions constrs cloNames cloTypes ccIds

reconnect :: MonadIO m => IORef AppState -> m ()
reconnect appStateRef = do
  state <- liftIO $ readIORef appStateRef
  case state ^. majorState of
    Connected socket debuggee' (PausedMode os) -> do
      let newAppState = updateAppState os (setTM SavedAndGCRoots) socket debuggee' state
      liftIO $ writeIORef appStateRef newAppState
    _ -> return ()

handleFilter :: MonadIO m => IORef AppState -> m ()
handleFilter appStateRef = do
  state <- liftIO $ readIORef appStateRef
  case state ^. majorState of
    Connected socket debuggee' (PausedMode os) -> do
      let mClosFilter = uiFiltersToFilter (_filters os)
      cps <- liftIO $ retainersOf (_resultSize os) mClosFilter Nothing debuggee'
      let cps' = map (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
      res <- liftIO $ mapM (mapM (completeClosureDetails debuggee')) cps'
      suggs <- getSuggestions mClosFilter debuggee'
      let tree = mkRetainerTree debuggee' res
      let newTM = Retainer tree suggs
          newAppState = updateAppState os (setTM newTM) socket debuggee' state
      liftIO $ writeIORef appStateRef newAppState

handleToggle newTree toggleIx selectedPath renderRow = do
  let mNode = getSubTree newTree toggleIx
  case mNode of
    Just (IOTreeNode _ (Right children)) -> do
      let htmlChildren = renderTreeNodesHtml renderRow selectedPath toggleIx children encodePath
          htmlResponse = renderText $ div_ htmlChildren
      Scotty.html htmlResponse    
    _ -> Scotty.html mempty

handlePartial appStateRef tree selectedPath getCDIO' renderSummary = do
  cdio <- getCDIO'
  updateImg appStateRef cdio
  let summaryHtml = renderText $ detailedSummary renderSummary tree selectedPath cdio
  let imgName' = imgName cdio
  let imgTitle = "Visualisation" ++ if null imgName' then "" else " of " ++ imgName'
  Scotty.json $ object ["summary" .= summaryHtml, "imgName" .= imgTitle]

app :: IORef AppState -> Scotty.ScottyM ()
app appStateRef = do
  {- Serves the visualisation of the selected object -}
  Scotty.get "/graph.svg" $ do
    Scotty.setHeader "Content-Type" "image/svg+xml"
    Scotty.file svgPath
  {- Generates the graph on demand -}
  Scotty.get "/graph" $ do
    -- these headers are required to prevent the browser from auto-caching stale images
    Scotty.addHeader "Cache-Control" "no-store, no-cache, must-revalidate, max-age=0"
    Scotty.addHeader "Pragma" "no-cache"
    Scotty.addHeader "Expires" "0"
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Connected _ _ (PausedMode os) -> do
        -- retrieve currently running task (if any) and set it to Nothing
        mOldTask <- liftIO $ atomicModifyIORef' appStateRef $ \st ->
          (st {currentTask = Nothing}, currentTask st)
        liftIO $ case mOldTask of
          Just oldTask -> do
            done <- poll oldTask
            case done of 
              Nothing -> cancel oldTask -- if the old task is still running, cancel it
              Just _ -> return ()
          Nothing -> return ()
        -- set the async graph gen in the IO, then wait for it to finish
        fastMode <- fastModeParam Scotty.queryParam
        newTask <- liftIO $ async $ _genSvg os (if fastMode then Sfdp else Dot) 
        liftIO $ atomicModifyIORef' appStateRef $ \st -> (st {currentTask = Just newTask}, ())
        result <- liftIO $ E.try (wait newTask) :: Scotty.ActionM (Either E.SomeException ())
        case result of
          Right _ -> do -- serve the file on successful completion
            Scotty.setHeader "Content-Type" "image/svg+xml"
            Scotty.file svgPath
          Left e -> do
            case E.fromException e of
              Just AsyncCancelled -> do -- error can be ignored, new graph is already loading
                Scotty.status status409
                Scotty.text "cancelled"
              _ -> do -- an actual error, display "Failed to load graph"
                Scotty.status status500
                liftIO $ putStrLn $ "Render task failed: " ++ show e
                Scotty.text "failed"
  {- Serves the profile dump file -}
  Scotty.get "/download-profile" $ do
    let filePath = "profile_dump"
    fileExists <- liftIO $ doesFileExist filePath
    if fileExists
      then do
        Scotty.setHeader "Content-Type" "application/octet-stream"
        Scotty.setHeader "Content-Disposition" "attachment; filename=\"profile_dump\""
        Scotty.file filePath
      else
        Scotty.text "Profile dump not found."
  {- Serves the ARR_WORDS histogram -}
  Scotty.get "/histogram" $ do
    Scotty.setHeader "Content-Type" "image/png"
    Scotty.file "tmp/histo.png"
  {- Main page where sockets/snapshots can be selected for debugging -}
  Scotty.get "/" $ do
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Setup st _ _ -> do
        newKnownDebuggees <- liftIO $ updateListFrom socketDirectory
        newKnownSnapshots <- liftIO $ updateListFrom snapshotDirectory
        let socketList = F.toList $ newKnownDebuggees
        let snapshotList = F.toList $ newKnownSnapshots
        liftIO $ writeIORef appStateRef $ state & majorState .~ Setup st newKnownDebuggees newKnownSnapshots
        case st of 
          Socket -> Scotty.html $ renderSocketSelectionPage st socketList
          Snapshot -> Scotty.html $ renderSocketSelectionPage st snapshotList
      Connected {} -> do
        Scotty.html $ renderAlreadyConnectedPage
  {- Pass eventlog to eventlog2html -}
  Scotty.post "/eventlogAnalysis" $ do
    file <- Scotty.files
    isJson <- eLogOutParam Scotty.formParam
    case lookup "eventlog" file of
      Just f -> do
        let path = "tmp/prog.eventlog"
            suffix = init $ last $ splitOn "." $ show $ fileName f
            isHeapProfile = if suffix == "hp" then True else False
        liftIO $ BS.writeFile path (fileContent f)
        let checkTraces _ = return ()
            args = EA.Args 
              { sorting = EA.Size
              , reversing = False
              , nBands = 15
              , detailedLimit = Nothing
              , heapProfile = isHeapProfile
              , noIncludejs = False
              , json = isJson
              , noTraces = False
              , traceEvents = False
              , userColourScheme = "category20b"
              , fixedYAxis = Nothing
              , includeStr = []
              , excludeStr = []
              , outputFile = Nothing
              , files = [path] 
              }
        if not isJson then do
          prof_type <- liftIO $ generateJsonValidate checkTraces path args
          let htmlStr = templateString prof_type args
          Scotty.html $ TL.pack htmlStr 
        else do
          Just (HeapProfileData val _ _) <- eventlogHeapProfile <$> liftIO (generateJson path args)
          let payload = encode val
          Scotty.setHeader "Content-Type" "application/json"
          Scotty.setHeader "Content-Disposition" "attachment; filename=\"eventlog.json\""
          Scotty.raw payload

          
      Nothing -> Scotty.text "No file uploaded"
  {- For fetching info so JS can update the page -}
  Scotty.get "/partial" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected socket debuggee' (PausedMode os) ->
        case _treeMode os of
          SavedAndGCRoots -> do
            let tree = _treeSavedAndGCRoots os
            let getCDIO' = getCDIO tree selectedPath (Just . closureName)
                           closureGetSize getNodeName closureFormat
            handlePartial appStateRef tree selectedPath getCDIO' renderClosureSummary
          Retainer tree suggs -> do
            let getCDIO' = getCDIO tree selectedPath (Just . closureName)
                           closureGetSize getNodeName closureFormat
            handlePartial appStateRef tree selectedPath getCDIO' renderClosureSummary

          SearchedHtml Utils{..} tree name -> do
            let nameFn = maybe "" id . _getName 
            let getCDIO' = getCDIO tree selectedPath _getName _getSize 
                           (\(IOTreeNode n' _) -> nameFn n') _graphFormat
            handlePartial appStateRef tree selectedPath getCDIO' _renderSummary


            
  {- GET version of /connect, in case / is accessed while already connected to a debuggee -}
  Scotty.get "/connect" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected socket debuggee' mode' -> 
        case mode' of 
          PausedMode os -> do
            let tree = _treeSavedAndGCRoots os
            cdio <- getCDIO tree selectedPath (Just . closureName) closureGetSize getNodeName closureFormat
            updateImg appStateRef cdio
            Scotty.html $ renderConnectedPage selectedPath cdio socket debuggee' mode'
          _ -> Scotty.html $ renderConnectedPage selectedPath (CDIO Nothing Nothing False) socket debuggee' mode'
      _ -> Scotty.redirect "/"
  {- Here debuggees can be paused and resumed. When paused, information about closures can be displayed -}
  Scotty.post "/connect" $ do
    reconnect appStateRef
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Setup st knownDebuggees' knownSnapshots' -> do
        socketParam <- Scotty.formParam "socket"
        case st of
          Snapshot -> do
            let snapshots = F.toList knownSnapshots'
            handleConnect appStateRef state socketParam snapshots (\ _ -> pure True) snapshotConnect 
          Socket -> do
            let sockets = F.toList knownDebuggees'
            handleConnect appStateRef state socketParam sockets
                          (\s -> isSocketAlive (_socketLocation s)) debuggeeConnect
      Connected _ _ _ -> do
        Scotty.redirect "/connect" 
  {- Toggles between socket and snapshot mode when selecting -}
  Scotty.post "/toggle-set-up" $ do
    liftIO $ modifyIORef' appStateRef $ \ state -> 
      state & majorState %~ \ st -> case st of 
        Setup st' d s -> Setup (toggleSetup st') d s
        other -> other
    Scotty.redirect "/"
  {- Pauses debuggee -}
  Scotty.post "/pause" $ do
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Connected _ debuggee' RunningMode -> do
        liftIO $ pause debuggee'
        ver <- liftIO $ GD.version debuggee'
        (rootsTree, initRoots) <- liftIO $ mkSavedAndGCRootsIOTree debuggee'
        let pausedState = PausedMode $
                            OperationalState
                              SavedAndGCRoots
                              (DefaultRoots initRoots)
                              rootsTree
                              (Just 100)
                              []
                              ver 
                              (const $ return ())
            newAppState = state & majorState . mode .~ pausedState
        liftIO $ writeIORef appStateRef newAppState
        Scotty.redirect "/connect"     
      _ -> Scotty.redirect "/"
  {- Resumes debuggee -}
  Scotty.post "/resume" $ do
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Connected _ debuggee' (PausedMode _) -> do
        liftIO $ resume debuggee'
        let newAppState = state & majorState . mode .~ RunningMode
        liftIO $ writeIORef appStateRef newAppState
        Scotty.redirect "/connect"
      _ -> Scotty.redirect "/"  
  {- Kill process or exit to selection screen -}
  Scotty.post "/exit" $ do
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Connected _ debuggee' (PausedMode _) -> do
        liftIO $ resume debuggee'
        let newAppState = initialAppState
        liftIO $ writeIORef appStateRef newAppState
        Scotty.redirect "/"
      _ -> Scotty.redirect "/"
  {- Toggles the expansion state of a path in the tree -}
  Scotty.get "/toggle" $ do
    toggleIx <- togglePathParam Scotty.queryParam
    selectedPath <- selectedParam Scotty.queryParam
    let newSelected = toggleSelected selectedPath toggleIx
    let selectedStr = encodePath newSelected
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Connected socket debuggee' (PausedMode os) -> do
        case _treeMode os of
          SavedAndGCRoots -> do 
            let tree = _treeSavedAndGCRoots os
            newTree <- liftIO $ toggleTreeByPath tree toggleIx
            let newAppState = state & majorState . mode . pausedMode . treeSavedAndGCRoots .~ newTree
            liftIO $ writeIORef appStateRef newAppState
            handleToggle newTree toggleIx selectedPath (detailedRowHtml renderClosureHtml "connect") 
          Retainer tree suggs -> do
            newTree <- liftIO $ toggleTreeByPath tree toggleIx
            let newAppState = updateAppState os (setTM $ Retainer newTree suggs) socket debuggee' state
            liftIO $ writeIORef appStateRef newAppState
            handleToggle newTree toggleIx selectedPath (detailedRowHtml renderClosureHtml "searchWithFilters") 
          SearchedHtml f tree name -> do
            newTree <- liftIO $ toggleTreeByPath tree toggleIx
            let newAppState = updateAppState os (setTM $ SearchedHtml f newTree name) socket debuggee' state
            liftIO $ writeIORef appStateRef newAppState
            handleToggle newTree toggleIx selectedPath (detailedRowHtml (_renderRow f) name) 
  {- View profile (level 1 and 2) -}
  genericGet appStateRef "/profile" renderProfilePage
  Scotty.post "/profile" $ do
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Connected socket debuggee' (PausedMode os) -> do
        level <- profileLevelParam Scotty.formParam
        profMap <- liftIO $ profile debuggee' level "profile_dump" 
        let sortedProfiles = Prelude.reverse $
              [ ProfileLine k kargs v
              | ((k, kargs), v) <- List.sortBy (comparing (cssize . snd)) (M.toList profMap)
              ]
            totalStats = foldMap snd (M.toList profMap)
        
            gChildren _ (ClosureLine c) = map ClosureLine <$> getChildren debuggee' c
            gChildren _ (ProfileLine _ _ stats) = do
              let samples = getSamples (sample stats)
              closures <- forM samples $ \ptr -> do
                deref <- run debuggee' $ GD.dereferenceClosure ptr
                return $ ListFullClosure $ Closure ptr deref
              filled <- forM (zip [0 :: Int ..] closures) $ \(i, c) -> do
                filledC <- fillListItem debuggee' c
                return (show i, filledC)
              mapM (\(lbl, filledItem) -> ClosureLine <$> getClosureDetails debuggee' (pack lbl) filledItem) filled
        let tree = mkIOTree debuggee' sortedProfiles gChildren id
        let profFormat x = case x of ClosureLine c -> closureFormat c; _ -> ""
        let getName' x = case x of ClosureLine c -> Just (closureName c); _ -> Nothing
        let getSize' x = case x of
                           ClosureLine (ClosureDetails _ excSize' _) -> getSize excSize'
                           _ -> 0
        let utils = Utils renderProfileHtml (renderProfileSummary totalStats) profFormat arrDumpProf getName' getSize'
            newTM = SearchedHtml utils tree "profile"
            newAppState = updateAppState os (setTM newTM) socket debuggee' state
        liftIO $ writeIORef appStateRef newAppState
        Scotty.redirect "/profile"
      _ -> Scotty.redirect "/"
  {- Downloads the arr_words payload -}
  Scotty.get "/dumpArrWords" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected _ _ (PausedMode os) -> do
        let handleTree :: IOTree a name -> (a -> ActionT IO ()) -> ActionT IO ()
            handleTree tree dump = case getSubTree tree selectedPath of
                                     Just (IOTreeNode node' _) -> dump node'
                                     Nothing -> error "Error: selected node doesn't exist"
        case _treeMode os of
          SavedAndGCRoots -> handleTree (_treeSavedAndGCRoots os) dumpArrWord
          Retainer tree _ -> handleTree tree dumpArrWord     
          SearchedHtml Utils{..} tree _ -> handleTree tree _dumpArrWords
      _ -> Scotty.redirect "/"
  {- See arr_words count -}
  genericGet appStateRef "/arrWordsCount" (renderCountPage "ARR_WORDS")
  Scotty.post "/arrWordsCount" $ do
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Connected socket debuggee' (PausedMode os) -> do
        arrMap <- liftIO $ arrWordsAnalysis Nothing debuggee'
        let all_res = Prelude.reverse $ 
              [ (k, S.toList v ) 
              | (k, v) <- (List.sortBy (comparing 
                                       (\(k, v) -> fromIntegral (BS.length k) * S.size v)) 
                                       (M.toList arrMap))
              ]
            display_res = maybe id take (_resultSize os) all_res
            top_closure = [CountLine k (fromIntegral (BS.length k)) (length v) | (k, v) <- display_res]

            !words_histogram = Just $ histogramHtml 8 (concatMap (\(k, bs) -> let sz = BS.length k in replicate (length bs) (Size (fromIntegral sz))) all_res)
            histogramImg f = renderHistogramImage f 8 (concatMap (\(k, bs) -> let sz = BS.length k in replicate (length bs) (Size (fromIntegral sz))) all_res)
 
            
            g_children d (CountLine b _ _) = do
              let cs = maybe Set.empty id (M.lookup b arrMap)
              cs' <- run debuggee' $ forM (S.toList cs) $ \c -> do
                c' <- GD.dereferenceClosure c
                return $ ListFullClosure $ Closure c c'
              children' <- traverse (traverse (fillListItem d)) $ zipWith (\n c -> (show @Int n, c)) [0..] cs'
              mapM (\(lbl, child) -> FieldLine <$> getClosureDetails d (pack lbl) child) children'
            g_children d (FieldLine c) = map FieldLine <$> getChildren d c

        eFont <- liftIO $ loadFontFile "DejaVuSans.ttf"
        case eFont of 
          Right font -> liftIO $ do 
            createDirectoryIfMissing True "tmp"
            BS.writeFile "tmp/histo.png" (histogramImg font)
            return ()
          Left _ -> error "Font file missing"

        let tree = mkIOTree debuggee' top_closure g_children id
        let (newTM, _) = countTM words_histogram tree "arrWordsCount"
            newAppState = updateAppState os (setTM newTM) socket debuggee' state
        liftIO $ writeIORef appStateRef newAppState
        Scotty.redirect "/arrWordsCount"
      _ -> Scotty.redirect "/"
  {- See strings count -}
  genericGet appStateRef "/stringsCount" (renderCountPage "Strings")
  Scotty.post "/stringsCount" $ do
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Connected socket debuggee' (PausedMode os) -> do
        stringMap <- liftIO $ stringsAnalysis Nothing debuggee'
        let sorted_res = maybe id take (_resultSize os) $ 
                         Prelude.reverse [(k, S.toList v ) 
                                         | (k, v) <- (List.sortBy (comparing (S.size . snd)) 
                                         (M.toList stringMap))]
        
            top_closure = [CountLine k (length k) (length v) | (k, v) <- sorted_res]
           
            g_children d (CountLine b _ _) = do
              let cs = maybe Set.empty id (M.lookup b stringMap)
              cs' <- run debuggee' $ forM (S.toList cs) $ \c -> do
                c' <- GD.dereferenceClosure c
                return $ ListFullClosure $ Closure c c'
              children' <- traverse (traverse (fillListItem d)) $ zipWith (\n c -> (show @Int n, c)) [0..] cs'
              mapM (\(lbl, child) -> FieldLine <$> getClosureDetails d (pack lbl) child) children'
            g_children d (FieldLine c) = map FieldLine <$> getChildren d c

        let tree = mkIOTree debuggee' top_closure g_children id
        let (newTM, _) = countTM Nothing tree "stringsCount"
            newAppState = updateAppState os (setTM newTM) socket debuggee' state
        liftIO $ writeIORef appStateRef newAppState
        Scotty.redirect "/stringsCount"
      _ -> Scotty.redirect "/" 
  {- Thunk analysis -}
  genericGet appStateRef "/thunkAnalysis" renderThunkAnalysisPage
  Scotty.post "/thunkAnalysis" $ do
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Connected socket debuggee' (PausedMode os) -> do
        thunkMap <- liftIO $ thunkAnalysis debuggee'
        let top_closure = Prelude.reverse [ ThunkLine k v | (k, v) <- (List.sortBy (comparing (getCount . snd)) (M.toList thunkMap))]

            g_children _ (ThunkLine {}) = pure []
        let tree = mkIOTree debuggee' top_closure g_children id
        let utils = Utils renderThunkAnalysisHtml renderThunkAnalysisSummary (\_->"") arrDumpThunk (\_->Nothing) (\_->0) 
        let newTM = SearchedHtml utils tree "thunkAnalysis"
            newAppState = updateAppState os (setTM newTM) socket debuggee' state
        liftIO $ writeIORef appStateRef newAppState
        Scotty.redirect "/thunkAnalysis"
      _ -> Scotty.redirect "/" 
  {- Take snapshot -}
  Scotty.post "/takeSnapshot" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.formParam
    filename <- Scotty.formParam "filename"
    case state ^. majorState of
      Connected _ debuggee' _ -> do
        liftIO $ snapshot debuggee' filename
        Scotty.redirect ("/connect?selected=" <> TL.fromStrict (encodePath selectedPath))
      Setup{} -> Scotty.redirect "/" 
  {- Search with filters -}
  genericGet appStateRef "/searchWithFilters" undefined
  Scotty.post "/searchWithFilters" $ do
    handleFilter appStateRef
    Scotty.redirect "/searchWithFilters"
  {- Adds selected filter to list -}
  Scotty.post "/addFilter" $ do
    state <- liftIO $ readIORef appStateRef
    pattern <- patternParam Scotty.formParam
    filterType <- filterTypeParam Scotty.formParam
    invert <- invertParam Scotty.formParam
    case state ^. majorState of 
      Connected socket debuggee' (PausedMode os) -> do 
        let newFilter = case filterType of 
                          "Address" -> maybe [] (pure . UIAddressFilter invert) (readClosurePtr pattern)
                          "InfoAddress" -> maybe [] (pure . UIInfoAddressFilter invert) (readInfoTablePtr pattern) 
                          "ConstrName" -> [UIConstructorFilter invert pattern]
                          "ClosureName" -> [UIInfoNameFilter invert pattern]
                          "ClosureSize" -> maybe [] (pure . UISizeFilter invert) (readMaybe pattern) 
                          "ClosureType" -> maybe [] (pure . UIClosureTypeFilter invert) (readMaybe pattern) 
                          "ARR_WORDSSize" -> maybe [] (\size -> 
                                                      [UIClosureTypeFilter False Debug.ARR_WORDS
                                                      , UISizeFilter False size]) 
                                                      (readMaybe pattern) 
                          "Era" -> maybe [] (pure . UIEraFilter invert) (parseEraRange $ T.pack pattern)
                          "CCID" -> maybe [] (pure . UICcId invert) (readMaybe pattern) 
                          _ -> []
        let newAppState = updateAppState os (addFilters newFilter) socket debuggee' state
        liftIO $ writeIORef appStateRef newAppState
        handleFilter appStateRef
        Scotty.redirect "/searchWithFilters?filterChanged=True"
      _ -> Scotty.redirect "/" 
  {- Deletes selected index from filters -}
  Scotty.post "/deleteFilter" $ do
    state <- liftIO $ readIORef appStateRef
    index <- indexParam Scotty.formParam
    case state ^. majorState of 
      Connected socket debuggee' (PausedMode os) -> do
        let newFilters = case index of
                           n | n < 0 -> _filters os 
                           _ -> let (as, bs) = Prelude.splitAt index (_filters os) 
                                in as ++ (tail bs)
            newAppState = updateAppState os (setFilters newFilters) socket debuggee' state
        liftIO $ writeIORef appStateRef newAppState
        handleFilter appStateRef
        Scotty.redirect "/searchWithFilters?filterChanged=True"
      _ -> Scotty.redirect "/" 
  {- Clears all filters -}
  Scotty.post "/clearFilters" $ do
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of 
      Connected socket debuggee' (PausedMode os) -> do
        let newAppState = updateAppState os (setFilters []) socket debuggee' state
        liftIO $ writeIORef appStateRef newAppState
        handleFilter appStateRef
        Scotty.redirect "/searchWithFilters?filterChanged=True"
      _ -> Scotty.redirect "/"
  {- Set the amount of results from filter searches etc. Input <= 0 -> no limit -}
  Scotty.post "/setSearchLimit" $ do
    state <- liftIO $ readIORef appStateRef
    limit <- searchLimitParam Scotty.formParam
    case state ^. majorState of 
      Connected socket debuggee' (PausedMode os) -> do
        let newResultSize = case limit of
                              Nothing -> _resultSize os
                              Just n | n <= 0 -> Nothing
                              n -> n
        let setLimit os' = os' { _resultSize = newResultSize }
            newAppState = updateAppState os setLimit socket debuggee' state
        liftIO $ writeIORef appStateRef newAppState
        Scotty.redirect "/connect"
      _ -> Scotty.redirect "/" 

  where mkSavedAndGCRootsIOTree debuggee' = do
          raw_roots <- take 1000 . map ("GC Roots",) <$> GD.rootClosures debuggee'
          rootClosures' <- liftIO $ mapM (completeClosureDetails debuggee') raw_roots
          raw_saved <- map ("Saved Object",) <$> GD.savedClosures debuggee'
          savedClosures' <- liftIO $ mapM (completeClosureDetails debuggee') raw_saved
          return $ (mkIOTree debuggee' (savedClosures' ++ rootClosures') getChildren id
                   , fmap toPtr <$> (raw_roots ++ raw_saved))


main :: IO ()
main = do
  let initial = initialAppState
  appStateRef <- newIORef initial
  Scotty.scotty 3000 $ do
    Scotty.middleware $ NWMS.staticPolicy (NWMS.addBase "src/js")
    app appStateRef
