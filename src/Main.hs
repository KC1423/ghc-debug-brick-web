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

import Filter
import Render
import Param
import Graph
import qualified Web.Scotty as Scotty
import Data.IORef
import qualified Data.Text.Lazy as TL
import Lucid
import qualified Control.Exception as E
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.GraphViz (GraphvizCommand(..), isGraphvizInstalled) 
import Web.Scotty.Internal.Types
import Network.Wai.Parse (FileInfo(..))
import Eventlog.Data (generateJsonValidate, HeapProfileData(..), eventlogHeapProfile, generateJson)
import Eventlog.HtmlTemplate (templateString)
import Graphics.Text.TrueType (loadFontFile) 
import qualified Eventlog.Args as EA
import Data.Aeson (encode, object, (.=))
import qualified Network.Wai.Middleware.Static as NWMS
import Control.Concurrent.Async
import Network.HTTP.Types (status409, status500)

import Control.Applicative
import Control.Monad (forM)
import Control.Monad.IO.Class
import qualified Data.List as List
import Data.Ord (comparing)
import qualified Data.Ord as Ord
import Lens.Micro.Platform ((^.), (&), (.~), (%~))
import System.Directory
import System.FilePath
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as S
import qualified Data.Foldable as F
import Text.Read (readMaybe)

import GHC.Debug.Profile.Types

import GHC.Debug.Types.Ptr(readInfoTablePtr, arrWordsBS)
import qualified GHC.Debug.Types.Closures as Debug
import IOTree
import Lib as GD
import Model

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
completeClosureDetails :: Debuggee -> (Text, DebugClosure CCSPtr SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr) -> IO ClosureDetails
completeClosureDetails dbg (label', clos) =
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
    , _info = InfoInfo
      { _pretty = pack pretty'
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
  return $ InfoInfo 
    { _pretty = pretty'
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

getClosureIncSize :: (a -> Maybe String) -> (a -> Int) -> Set.Set String -> IOTreeNode (a, [Int], Bool) name -> Int
getClosureIncSize getName' getSize' seen' node' = fst (go seen' node')
  where
    go seen (IOTreeNode (n, _, _) csE) =
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
                 cdio <- getCDIO tree' selectedPath (Just . closureName) closureGetSize closureFormat []
                 updateImg appStateRef cdio []
                 Scotty.html $ renderFilterSearchPage tree' suggs (_filters os) (_version os) selectedPath cdio
               SearchedHtml u@(Utils{..}) tree name -> do
                 cdio <- getCDIO tree selectedPath _getName _getSize _graphFormat []
                 updateImg appStateRef cdio []
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

updateImg :: IORef AppState -> CDIO -> [[Int]] -> Scotty.ActionM ()
updateImg appStateRef (CDIO _ (Just ImgInfo{..}) _) forced = do
  state <- liftIO $ readIORef appStateRef
  case state ^. majorState of
    Connected s d (PausedMode os) -> do
      let newOs = os { _genSvg = _svgContent, _forceExpand = forced }
      let newState = state { _majorState = Connected s d (PausedMode newOs) } 
      liftIO $ writeIORef appStateRef newState
      return ()
    _ -> Scotty.redirect "/"
updateImg _ _ _ = return ()

getCDIO :: IOTree a name -> [Int] -> (a -> Maybe String) -> (a -> Int) -> (a -> String) -> [[Int]]
        -> ActionT IO CDIO
getCDIO tree selectedPath getName' getSize' format' forced = 
  case getSubTree tree selectedPath of
    Nothing -> do
      hasGV <- liftIO $ isGraphvizInstalled
      return $ CDIO Nothing Nothing hasGV
    Just subtree -> do
      (expSubTree, capped) <- liftIO $ expandNodeSafe subtree (maybe "" id . getName') forced
      let mInc = Just (getClosureIncSize getName' getSize' Set.empty expSubTree, capped)
      hasGV <- liftIO $ isGraphvizInstalled
      if not hasGV then return $ CDIO mInc Nothing hasGV
      else do
        imgInfo <- handleImg expSubTree capped getName' format'
        return $ CDIO mInc imgInfo hasGV

handleImg :: IOTreeNode (a, [Int], Bool) name -> Bool -> (a -> Maybe String) -> (a -> String) -> Scotty.ActionM (Maybe ImgInfo)
handleImg expSubtree@(IOTreeNode (n', _, _) _) capped getName'' format' = do
  case getName'' n' of
    Just _ -> do 
      let getName' = maybe "" id . getName''
      let name = getName' n'
      let (nodes', fNodes, vizEdges) = getClosureVizTree getName' format' Set.empty [] [] expSubtree
      let vizNodes = Set.toList nodes'
      let graph = buildClosureGraph vizNodes fNodes vizEdges
      let svgContent comm = liftIO $ do 
                              let tweakGraph = case comm of
                                                 Dot -> id
                                                 _ -> noNodeOverlap
                              createDirectoryIfMissing True "tmp"
                              _ <- graphvizProcess comm svgPath (tweakGraph graph)
                              return ()
      return $ Just $ ImgInfo name capped svgContent
    Nothing -> return Nothing

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

reconnect :: MonadIO m => IORef AppState -> m ()
reconnect appStateRef = do
  state <- liftIO $ readIORef appStateRef
  case state ^. majorState of
    Connected socket debuggee' (PausedMode os) -> do
      let newAppState = updateAppState os (setTM SavedAndGCRoots) socket debuggee' state
      liftIO $ writeIORef appStateRef newAppState
    _ -> return ()

handleFilter :: IORef AppState -> Scotty.ActionM ()
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
    _ -> Scotty.redirect "/"

handleToggle :: IOTree a name -> [Int] -> [Int] -> ([Int] -> Bool -> Bool -> a -> Html ())
             -> Scotty.ActionM ()
handleToggle newTree toggleIx selectedPath renderRow = do
  let mNode = getSubTree newTree toggleIx
  case mNode of
    Just (IOTreeNode _ (Right children)) -> do
      let htmlChildren = renderTreeNodesHtml renderRow selectedPath toggleIx children encodePath
          htmlResponse = renderText $ div_ htmlChildren
      Scotty.html htmlResponse    
    _ -> Scotty.html mempty

handlePartial :: (Ord name, Show name)
              => IORef AppState -> IOTree a name -> [Int] -> ActionT IO CDIO
              -> (a -> [Int] -> CDIO -> Html ()) -> ActionT IO ()
handlePartial appStateRef tree selectedPath getCDIO' renderSummary = do
  cdio <- getCDIO'
  updateImg appStateRef cdio []
  let summaryHtml = renderText $ detailedSummary renderSummary tree selectedPath cdio
  let imgName' = imgName cdio
  let imgTitle = "Visualisation" ++ if null imgName' then "" else " of " ++ imgName'
  let capWarning = if imgCap cdio then "Note: this is a very large object, and this graph is incomplete" else "" :: String
  Scotty.json $ object ["summary" .= summaryHtml, "imgName" .= imgTitle, "capWarning" .= capWarning]

renderGraph :: IORef AppState -> OperationalState -> ActionT IO ()
renderGraph appStateRef os = do 
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
      Connected _ _ (PausedMode os) -> renderGraph appStateRef os
      _ -> Scotty.redirect "/"
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
    sortOn <- eLogSortParam Scotty.formParam
    rev <- eLogRevParam Scotty.formParam
    case lookup "eventlog" file of
      Just f -> do
        let path = "tmp/prog.eventlog"
            suffix = init $ last $ splitOn "." $ show $ fileName f
            isHeapProfile = suffix == "hp"
        liftIO $ BS.writeFile path (fileContent f)
        let checkTraces _ = return ()
            args = EA.Args 
              { sorting = sortOn
              , reversing = rev
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
      Connected _ _ (PausedMode os) ->
        case _treeMode os of
          SavedAndGCRoots -> do
            let tree = _treeSavedAndGCRoots os
            let getCDIO' = getCDIO tree selectedPath (Just . closureName)
                           closureGetSize closureFormat []
            handlePartial appStateRef tree selectedPath getCDIO' renderClosureSummary
          Retainer tree _ -> do
            let getCDIO' = getCDIO tree selectedPath (Just . closureName)
                           closureGetSize closureFormat []
            handlePartial appStateRef tree selectedPath getCDIO' renderClosureSummary

          SearchedHtml Utils{..} tree _ -> do
            let getCDIO' = getCDIO tree selectedPath _getName _getSize _graphFormat []
            handlePartial appStateRef tree selectedPath getCDIO' _renderSummary
      _ -> Scotty.redirect "/"
  {- For fetching additional info for graphs -}
  Scotty.get "/forceExpand" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    forcePath <- forceParam Scotty.queryParam
    case state ^. majorState of
      Connected _ _ (PausedMode os) -> do
        let forced = forcePath : _forceExpand os
        case _treeMode os of
          SavedAndGCRoots -> do
            let tree = _treeSavedAndGCRoots os
            cdio <- getCDIO tree selectedPath (Just . closureName) closureGetSize closureFormat forced
            updateImg appStateRef cdio forced      
          Retainer tree _ -> do
            cdio <- getCDIO tree selectedPath (Just . closureName) closureGetSize closureFormat forced
            updateImg appStateRef cdio forced      
          SearchedHtml Utils{..} tree _ -> do
            cdio <- getCDIO tree selectedPath _getName _getSize _graphFormat forced
            updateImg appStateRef cdio forced      
        newState <- liftIO $ readIORef appStateRef
        case newState ^. majorState of
          Connected _ _ (PausedMode newOs) -> renderGraph appStateRef newOs
          _ -> Scotty.redirect "/"
      _ -> Scotty.redirect "/"
  {- GET version of /connect, in case / is accessed while already connected to a debuggee -}
  Scotty.get "/connect" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected socket debuggee' mode' -> 
        case mode' of 
          PausedMode os -> do
            let tree = _treeSavedAndGCRoots os
            cdio <- getCDIO tree selectedPath (Just . closureName) closureGetSize closureFormat []
            updateImg appStateRef cdio []
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
                              [[]]
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
      _ -> Scotty.redirect "/"
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
              filled' <- forM (zip [0 :: Int ..] closures) $ \(i, c) -> do
                filledC <- fillListItem debuggee' c
                return (show i, filledC)
              mapM (\(lbl, filledItem) -> ClosureLine <$> getClosureDetails debuggee' (pack lbl) filledItem) filled'
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

        eFont <- liftIO $ loadFontFile fontPath --"src/DejaVuSans.ttf"
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
  Scotty.get "/takeSnapshot" $ do
    state <- liftIO $ readIORef appStateRef
    filename <- Scotty.queryParam "filename"
    case state ^. majorState of
      Connected _ debuggee' _ -> do
        liftIO $ snapshot debuggee' filename
        Scotty.text $ "Saved snapshot named: " <> TL.pack filename 
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
                                in as ++ (drop 1 bs)
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
  Scotty.get "/setSearchLimit" $ do
    state <- liftIO $ readIORef appStateRef
    limit <- searchLimitParam Scotty.queryParam
    case state ^. majorState of 
      Connected socket debuggee' (PausedMode os) -> do
        let newResultSize = case limit of
                              Nothing -> _resultSize os
                              Just n | n <= 0 -> Nothing
                              n -> n
        let setLimit os' = os' { _resultSize = newResultSize }
            newAppState = updateAppState os setLimit socket debuggee' state
        liftIO $ writeIORef appStateRef newAppState
        Scotty.text $ "Updated limit to: " <> case newResultSize of 
                                                Nothing -> "unlimited"
                                                Just n -> TL.pack $ show n
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
