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

import Brick
import Brick.BChan
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center (centerLayer, hCenter)
import Brick.Widgets.List
import Control.Applicative
import Control.Monad (forever, forM)
import Control.Monad.IO.Class
import Control.Monad.Catch (bracket)
import Control.Concurrent
import qualified Data.List as List
import Data.Ord (comparing)
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.CrossPlatform as Vty
import Graphics.Vty.Input.Events (Key(..))
import Lens.Micro.Platform
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
import Data.Time.Format
import Data.Time.Clock
import qualified Numeric


drawSetup :: Text -> Text -> GenericList Name Seq.Seq SocketInfo -> Widget Name
drawSetup herald other_herald vals =
      let nKnownDebuggees = Seq.length $ (vals ^. listElementsL)
      in mainBorder "ghc-debug" $ vBox
        [ hBox
          [ txt $ "Select a " <> herald <> " to debug (" <> pack (show nKnownDebuggees) <> " found):"
          ]
        , renderList
            (\elIsSelected socketPath -> (if elIsSelected then highlighted else id) $ hBox
                [ txt (socketName socketPath)
                , txt " - "
                , txt (renderSocketTime socketPath)
                ]
            )
            True
            vals
        , vLimit 1 $ withAttr menuAttr $ hBox [txt $ "(ESC): exit | (TAB): toggle " <> other_herald <> " view", fill ' ']
        ]

mainBorder :: Text -> Widget a -> Widget a
mainBorder title w = -- borderWithLabel (txt title) . padAll 1
  vLimit 1 (withAttr menuAttr $ hCenter $ fill ' ' <+> txt title <+> fill ' ') <=> w

myAppDraw :: AppState -> [Widget Name]
myAppDraw (AppState majorState' _) =
    case majorState' of

    Setup setupKind' dbgs snaps ->
      case setupKind' of
        Socket -> [drawSetup "process" "snapshots" dbgs]
        Snapshot -> [drawSetup "snapshot" "processes" snaps]


    Connected socket _debuggee mode' -> case mode' of

      RunningMode -> [mainBorder ("ghc-debug - Running - " <> socketName socket) $ vBox
        [ txtWrap "There is nothing you can do until the process is paused by pressing (p) ..."
        , fill ' '
        , withAttr menuAttr $ vLimit 1 $ hBox [txt "(p): Pause | (ESC): Exit", fill ' ']
        ]]

      (PausedMode os@(OperationalState _ last_task treeMode' kbmode fmode _ _ _ _ rfilters debuggeeVersion)) -> let
           last_task_string =
            case last_task of
              Nothing -> ""
              Just (d,t) -> " - " <> d <> " (" <> T.pack (formatTime defaultTimeLocale "%2Es" t) <> "s)"

        in kbOverlay kbmode debuggeeVersion
          $ [mainBorder ("ghc-debug - Paused - " <> socketName socket <> last_task_string) $ vBox
          [ -- Current closure details
              joinBorders $ (borderWithLabel (txt "Closure Details") $
              (vLimit 9 $
                pauseModeTree (\r io -> maybe emptyWidget r (ioTreeSelection io)) os
                <=> fill ' '))
              <+> (filterWindow rfilters)
          , -- Tree
            joinBorders $ borderWithLabel
              (txt $ case treeMode' of
                SavedAndGCRoots {} -> "Root Closures"
                Retainer {} -> "Retainers"
                Searched {} -> "Search Results"
              )
              (pauseModeTree (\_ -> renderIOTree) os)
          , footer (osSize os) (_resultSize os) fmode
          ]]

  where

  kbOverlay :: OverlayMode -> GD.Version -> [Widget Name] -> [Widget Name]
  kbOverlay KeybindingsShown _ ws = centerLayer kbWindow : ws
  kbOverlay (CommandPicker inp cmd_list _) debuggeeVersion ws  = centerLayer (cpWindow debuggeeVersion inp cmd_list) : ws
  kbOverlay NoOverlay _ ws = ws

  filterWindow [] = emptyWidget
  filterWindow xs = borderWithLabel (txt "Filters") $ hLimit 50 $ vBox $ map renderUIFilter xs

  cpWindow :: GD.Version -> Form Text () Name -> GenericList Name Seq.Seq Command -> Widget Name
  cpWindow debuggeeVersion input cmd_list = hLimit (actual_width + 2) $ vLimit (length commandList + 4) $
    withAttr menuAttr $
    borderWithLabel (txt "Command Picker") $ vBox $
      [ renderForm input
      , renderList (\elIsSelected -> if elIsSelected then highlighted . renderCommand debuggeeVersion else renderCommand debuggeeVersion) False cmd_list]

  kbWindow :: Widget Name
  kbWindow =
    withAttr menuAttr $
    borderWithLabel (txt "Keybindings") $ vBox $
      map renderCommandDesc all_keys

  all_keys =
    [ ("Resume", Just (Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]))
    , ("Parent", Just (Vty.EvKey KLeft []))
    , ("Child", Just (Vty.EvKey KRight []))
    , ("Command Picker", Just (Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl]))
    , ("Invert Filter", Just invertFilterEvent)]
    ++ [(commandDescription cmd, commandKey cmd) | cmd <- F.toList commandList ]
    ++ [ ("Exit", Just (Vty.EvKey KEsc [])) ]

  maximum_size = maximum (map (T.length . fst) all_keys)

  actual_width = maximum_size + 5  -- 5, maximum width of rendering a key
                              + 1  -- 1, at least one padding

  renderKey :: Vty.Event -> Text
  renderKey (Vty.EvKey (KFun n) []) = "(F" <> T.pack (show n) <> ")"
  renderKey (Vty.EvKey k [Vty.MCtrl]) = "(^" <> renderNormalKey k <> ")"
  renderKey (Vty.EvKey k [])       = "(" <> renderNormalKey k <> ")"
  renderKey _k = "()"

  renderNormalKey (KChar c) = T.pack [c]
  renderNormalKey KEsc = "ESC"
  renderNormalKey KLeft = "←"
  renderNormalKey KRight = "→"
  renderNormalKey _k = "�"

  mayDisableMenuItem debuggeeVersion cmd
    | isCmdDisabled debuggeeVersion cmd = disabledMenuItem
    | otherwise = id

  renderCommand debuggeeVersion cmd =
    mayDisableMenuItem debuggeeVersion cmd $
    renderCommandDesc (commandDescription cmd, commandKey cmd)

  renderCommandDesc :: (Text, Maybe Vty.Event) -> Widget Name
  renderCommandDesc (desc, k) = txt (desc <> T.replicate padding " " <> key)
    where
      key = maybe mempty renderKey k
      padding = (actual_width - T.length desc - T.length key)

renderInfoInfo :: InfoInfo -> [Widget Name]
renderInfoInfo info' =
  maybe [] renderSourceInformation (_sourceLocation info')
    ++ profHeaderInfo
    -- TODO these aren't actually implemented yet
    -- , txt $ "Type             "
    --       <> fromMaybe "" (_closureType =<< cd)
    -- , txt $ "Constructor      "
    --       <> fromMaybe "" (_constructor =<< cd)
  where
    profHeaderInfo = case _profHeaderInfo info' of
      Just x ->
        let plabel = case x of
              Debug.RetainerHeader{} -> "Retainer info"
              Debug.LDVWord{} -> "LDV info"
              Debug.EraWord{} -> "Era"
              Debug.OtherHeader{} -> "Other"
        in [labelled plabel $ vLimit 1 (txt $ renderProfHeaderInline x)]
      Nothing -> []

    renderProfHeaderInline :: ProfHeaderWord -> Text
    renderProfHeaderInline pinfo =
      case pinfo of
        Debug.RetainerHeader {} -> pack (show pinfo) -- This should never be visible
        Debug.LDVWord {state, creationTime, lastUseTime} ->
          (if state then "✓" else "✘") <> " created: " <> pack (show creationTime) <> (if state then " last used: " <> pack (show lastUseTime) else "")
        Debug.EraWord era -> pack (show era)
        Debug.OtherHeader other -> "Not supported: " <> pack (show other)

renderSourceInformation :: SourceInformation -> [Widget Name]
renderSourceInformation (SourceInformation name cty ty label' modu loc) =
    [ labelled "Name" $ vLimit 1 (str name)
    , labelled "Closure type" $ vLimit 1 (str (show cty))
    , labelled "Type" $ vLimit 3 (str ty)
    , labelled "Label" $ vLimit 1 (str label')
    , labelled "Module" $ vLimit 1 (str modu)
    , labelled "Location" $ vLimit 1 (str loc)
    ]

labelled :: Text -> Widget Name -> Widget Name
labelled = labelled' 20

labelled' :: Int -> Text -> Widget Name -> Widget Name
labelled' leftSize lbl w =
  hLimit leftSize  (txtLabel lbl <+> vLimit 1 (fill ' ')) <+> w <+> vLimit 1 (fill ' ')

renderUIFilter :: UIFilter -> Widget Name
renderUIFilter (UIAddressFilter invert x)     = labelled (bool "" "!" invert <> "Closure address") (str (show x))
renderUIFilter (UIInfoAddressFilter invert x) = labelled (bool "" "!" invert <> "Info table address") (str (show x))
renderUIFilter (UIConstructorFilter invert x) = labelled (bool "" "!" invert <> "Constructor name") (str x)
renderUIFilter (UIInfoNameFilter invert x)    = labelled (bool "" "!" invert <> "Constructor name (exact)") (str x)
renderUIFilter (UIEraFilter invert  x)        = labelled (bool "" "!" invert <> "Era range") (str (showEraRange x))
renderUIFilter (UISizeFilter invert x)        = labelled (bool "" "!" invert <> "Size (lower bound)") (str (show $ getSize x))
renderUIFilter (UIClosureTypeFilter invert x) = labelled (bool "" "!" invert <> "Closure type") (str (show x))
renderUIFilter (UICcId invert x)              = labelled (bool "" "!" invert <> "CC Id") (str (show x))


renderClosureDetails :: ClosureDetails -> Widget Name
renderClosureDetails (cd@(ClosureDetails {})) =
  vLimit 8 $
  -- viewport Connected_Paused_ClosureDetails Both $
  vBox $
    renderInfoInfo (_info cd)
    ++
    [ hBox
      [ txtLabel "Exclusive Size" <+> vSpace <+> renderBytes (GD.getSize $ _excSize cd)
      ]
    ]
renderClosureDetails ((LabelNode n)) = txt n
renderClosureDetails ((InfoDetails info')) = vLimit 8 $ vBox $ renderInfoInfo info'
renderClosureDetails (CCSDetails _ _ptr (Debug.CCSPayload{..})) = vLimit 8 $ vBox $
  [ labelled "ID" $ vLimit 1 (str $ show ccsID)
  ] ++ renderCCPayload ccsCc
renderClosureDetails (CCDetails _ c) = vLimit 8 $ vBox $ renderCCPayload c

renderCCPayload :: CCPayload -> [Widget Name]
renderCCPayload Debug.CCPayload{..} =
  [ labelled "Label" $ vLimit 1 (str ccLabel)
  , labelled "CC ID" $ vLimit 1 (str $ show ccID)
  , labelled "Module" $ vLimit 1 (str ccMod)
  , labelled "Location" $ vLimit 1 (str ccLoc)
  , labelled "Allocation" $ vLimit 1 (str $ show ccMemAlloc)
  , labelled "Time Ticks" $ vLimit 1 (str $ show ccTimeTicks)
  , labelled "Is CAF" $ vLimit 1 (str $ show ccIsCaf)
  ]

renderBytes :: Real a => a -> Widget n
renderBytes n =
  str (getShortHand (getAppropriateUnits (ByteValue (realToFrac n) Bytes)))



footer :: Int -> Maybe Int -> FooterMode -> Widget Name
footer n m fmode = vLimit 1 $
 case fmode of
   FooterMessage t -> withAttr menuAttr $ hBox [txt t, fill ' ']
   FooterInfo -> withAttr menuAttr $ hBox $ [padRight Brick.Max $ txt "(↑↓): select item | (→): expand | (←): collapse | (^p): command picker | (^g): invert filter | (?): full keybindings"]
                                         ++ [padLeft (Pad 1) $ str $
                                               (show n <> " items/" <> maybe "∞" show m <> " max")]
   FooterInput _im form -> renderForm form

footerInput :: FooterInputMode -> FooterMode
footerInput im =
  FooterInput im (footerInputForm im)

footerInputForm :: FooterInputMode -> Form Text e Name
footerInputForm im =
  newForm [(\w -> txtLabel (formatFooterMode im) <+> forceAttr inputAttr w) @@= editTextField id Footer (Just 1)] ""

updateListFrom :: MonadIO m =>
                        IO FilePath
                        -> GenericList n Seq.Seq SocketInfo
                        -> m (GenericList n Seq.Seq SocketInfo)
updateListFrom dirIO llist = liftIO $ do
            dir :: FilePath <- dirIO
            debuggeeSocketFiles :: [FilePath] <- listDirectory dir <|> return []

            -- Sort the sockets by the time they have been created, newest
            -- first.
            debuggeeSockets <- List.sortBy (comparing Ord.Down)
                                  <$> mapM (mkSocketInfo . (dir </>)) debuggeeSocketFiles

            let currentSelectedPathMay :: Maybe SocketInfo
                currentSelectedPathMay = fmap snd (listSelectedElement llist)

                newSelection :: Maybe Int
                newSelection = do
                  currentSelectedPath <- currentSelectedPathMay
                  List.findIndex ((currentSelectedPath ==)) debuggeeSockets

            return $ listReplace
                      (Seq.fromList debuggeeSockets)
                      (newSelection <|> (if Prelude.null debuggeeSockets then Nothing else Just 0))
                      llist


myAppHandleEvent :: BrickEvent Name Event -> EventM Name AppState ()
myAppHandleEvent brickEvent = do
  appState@(AppState majorState' eventChan) <- get
  case brickEvent of
    _ -> case majorState' of
      Setup st knownDebuggees' knownSnapshots' -> case brickEvent of

        VtyEvent (Vty.EvKey KEsc _) -> halt
        VtyEvent event -> case event of
          -- Connect to the selected debuggee
          Vty.EvKey (KChar '\t') [] -> do
            put $ appState & majorState . setupKind %~ toggleSetup
          Vty.EvKey KEnter _ ->
            case st of
              Snapshot
                | Just (_debuggeeIx, socket) <- listSelectedElement knownSnapshots'
                -> do
                  debuggee' <- liftIO $ snapshotConnect (writeBChan eventChan . ProgressMessage) (view socketLocation socket)
                  put $ appState & majorState .~ Connected
                        { _debuggeeSocket = socket
                        , _debuggee = debuggee'
                        , _mode     = RunningMode  -- TODO should we query the debuggee for this?
                    }
              Socket
                | Just (_debuggeeIx, socket) <- listSelectedElement knownDebuggees'
                -> do
                  bracket
                    (liftIO $ debuggeeConnect (writeBChan eventChan . ProgressMessage) (view socketLocation socket))
                    (\debuggee' -> liftIO $ resume debuggee')
                    (\debuggee' ->
                      put $ appState & majorState .~ Connected
                        { _debuggeeSocket = socket
                        , _debuggee = debuggee'
                        , _mode     = RunningMode  -- TODO should we query the debuggee for this?
                        })
              _ -> return ()

          -- Navigate through the list.
          _ -> do
            case st of
              Snapshot -> do
                zoom (majorState . knownSnapshots) (handleListEventVi handleListEvent event)
              Socket -> do
                zoom (majorState . knownDebuggees) (handleListEventVi handleListEvent event)

        AppEvent event -> case event of
          PollTick -> do
            -- Poll for debuggees
            knownDebuggees'' <- updateListFrom socketDirectory knownDebuggees'
            knownSnapshots'' <- updateListFrom snapshotDirectory knownSnapshots'
            put $ appState & majorState . knownDebuggees .~ knownDebuggees''
                                & majorState . knownSnapshots .~ knownSnapshots''
          _ -> return ()
        _ -> return ()

      Connected _socket' debuggee' mode' -> case mode' of

        RunningMode -> case brickEvent of
          -- Exit
          VtyEvent (Vty.EvKey KEsc _) ->
            halt
          -- Pause the debuggee
          VtyEvent (Vty.EvKey (KChar 'p') []) -> do
            liftIO $ pause debuggee'
            ver <- liftIO $ GD.version debuggee'
            (rootsTree, initRoots) <- liftIO $ mkSavedAndGCRootsIOTree
            put (appState & majorState . mode .~
                        PausedMode
                          (OperationalState Nothing
                                            Nothing
                                            savedAndGCRoots
                                            NoOverlay
                                            FooterInfo
                                            (DefaultRoots initRoots)
                                            rootsTree
                                            eventChan
                                            (Just 100)
                                            []
                                            ver))



          _ -> return ()

        PausedMode os -> case brickEvent of
          _ -> case brickEvent of
              -- Resume the debuggee if '^r', exit if ESC
              VtyEvent (Vty.EvKey (KChar 'r') [Vty.MCtrl]) -> do
                  liftIO $ resume debuggee'
                  put (appState & majorState . mode .~ RunningMode)
              VtyEvent (Vty.EvKey (KEsc) _) | NoOverlay <- view keybindingsMode os
                                            , not (isFocusedFooter (view footerMode os)) -> do
                  case view running_task os of
                    Just tid -> do
                      liftIO $ killThread tid
                      put $ appState & majorState . mode . pausedMode . running_task .~ Nothing
                                     & majorState . mode . pausedMode %~ resetFooter
                    Nothing -> do
                      liftIO $ resume debuggee'
                      put $ initialAppState (_appChan appState)

              -- handle any other more local events; mostly key events
              _ -> liftHandler (majorState . mode) os PausedMode (handleMain debuggee')
                     (brickEvent)



        where


        mkSavedAndGCRootsIOTree = do
          raw_roots <- take 1000 . map ("GC Roots",) <$> GD.rootClosures debuggee'
          rootClosures' <- liftIO $ mapM (completeClosureDetails debuggee') raw_roots
          raw_saved <- map ("Saved Object",) <$> GD.savedClosures debuggee'
          savedClosures' <- liftIO $ mapM (completeClosureDetails debuggee') raw_saved
          return $ (mkIOTree debuggee' (savedClosures' ++ rootClosures') getChildren renderInlineClosureDesc id
                   , fmap toPtr <$> (raw_roots ++ raw_saved))


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


fillListItem :: Debuggee
             -> ListItem CCSPtr SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr
             -> IO (ListItem CCSPtr SrtCont PayloadCont ConstrDesc StackCont ClosurePtr)
fillListItem _ (ListOnlyInfo x) = return $ ListOnlyInfo x
fillListItem d (ListFullClosure cd) = ListFullClosure <$> fillConstrDesc d cd
fillListItem _ ListData = return ListData
fillListItem _ (ListCCS c1 c2) = return $ ListCCS c1 c2
fillListItem _ (ListCC c1) = return $ ListCC c1

mkIOTree :: Debuggee
         -> [a]
         -> (Debuggee -> a -> IO [a])
         -> (a -> [Widget Name])
-- -> IO [(String, ListItem SrtCont PayloadCont ConstrDesc StackCont ClosurePtr)])
         -> ([a] -> [a])
         -> IOTree a Name
mkIOTree debuggee' cs getChildrenGen renderNode sort = ioTree Connected_Paused_ClosureTree
        (sort cs)
        (\c -> sort <$> getChildrenGen debuggee' c
--            cDets <- mapM (\(lbl, child) -> getClosureDetails debuggee' manalysis (pack lbl) child) children
--            return (sort cDets)
        )
        -- rendering the row
        (\state selected ctx depth closureDesc ->
          let
            body =
              (if selected then visible . highlighted else id) $
                hBox $
                renderNode closureDesc
          in
            vdecorate state ctx depth body -- body (T.concat context)
        )

era_colors :: [Vty.Color]
era_colors = [Vty.Color240 n | n <- [17..230]]

grey :: Vty.Color
grey = Vty.rgbColor (158 :: Int) 158 158

-- | Draw the tree structure around the row item. Inspired by the
-- 'border' functions in brick.
--
vdecorate :: RowState -> RowCtx -> [RowCtx] -> Widget n -> Widget n
vdecorate state ctx depth body =
  Widget Fixed Fixed $ do
    c <- getContext

    let decorationWidth = 2 * length depth + 4

    bodyResult <-
      render $
      hLimit (c ^. availWidthL - decorationWidth) $
      vLimit (c ^. availHeightL) $
      body

    let leftTxt =
          T.concat $
          map
            (\ x -> case x of
              LastRow -> "  "
              NotLastRow -> "│ "
            )
          (List.reverse depth)
        leftPart = withAttr treeAttr (vreplicate leftTxt)
        middleTxt1 =
          case ctx of
            LastRow -> "└─"
            NotLastRow -> "├─"
        middleTxt1' =
          case ctx of
            LastRow -> "  "
            NotLastRow -> "│ "
        middleTxt2 =
          case state of
            Expanded True -> "● " -- "⋅"
            Expanded False -> "┐ "
            Collapsed -> "┄ "
        middleTxt2' =
          case state of
            Expanded True -> "  "
            Expanded False -> "│ "
            Collapsed -> "  "
        middlePart =
          withAttr treeAttr $
            (txt middleTxt1 <=> vreplicate middleTxt1')
            <+> (txt middleTxt2 <=> vreplicate middleTxt2')
        rightPart = Widget Fixed Fixed $ return bodyResult
        total = leftPart <+> middlePart <+> rightPart

    render $
      hLimit (bodyResult ^. imageL . to Vty.imageWidth + decorationWidth) $
      vLimit (bodyResult ^. imageL . to Vty.imageHeight) $
      total

vreplicate :: Text -> Widget n
vreplicate t =
  Widget Fixed Greedy $ do
    c <- getContext
    return $ emptyResult & imageL .~ Vty.vertCat (replicate (c ^. availHeightL) (Vty.text' (c ^. attrL) t))
{-
  hBox
    [ withAttr treeAttr $ Widget Fixed Fixed $ do
        c <- getContext
        limitedResult <- render (hLimit (c ^. availWidthL - T.length t) $ vLimit (c ^. availHeightL) $ body)
        return $ emptyResult & imageL .~ vertCat (replicate (limitedResult ^. imageL . to imageHeight) (text' (c ^. attrL) t))
    , body
    ]
  where
    bodyWidth =
      render (hLimit (c ^. availWidthL - (length depth * 2 + 4)) $ vLimit (c ^. availHeightL) $ body)
-}

renderInlineClosureDesc :: ClosureDetails -> [Widget n]
renderInlineClosureDesc (LabelNode t) = [txtLabel t]
renderInlineClosureDesc (InfoDetails info') =
  [txtLabel (_labelInParent info'), vSpace, txt (_pretty info')]
renderInlineClosureDesc (CCSDetails clabel _cptr ccspayload) =
  [ txtLabel clabel, vSpace, txt (prettyCCS ccspayload)]
renderInlineClosureDesc (CCDetails clabel cc) =
  [ txtLabel clabel, vSpace, txt (prettyCC cc)]
renderInlineClosureDesc closureDesc@(ClosureDetails{}) =
                    [ txtLabel (_labelInParent (_info closureDesc))
                    , colorBar
                    , txt $  pack (closureShowAddress (_closure closureDesc))
                    , vSpace
                    , txtWrap $ _pretty (_info closureDesc)
                    ]
  where
    colorBar =
      case colorId of
        Just {} -> padLeftRight 1 (colorEra (txt " "))
        Nothing -> vSpace

    colorId = _profHeaderInfo $ _info closureDesc
    colorEra = case colorId of
      Just (Debug.EraWord i) -> modifyDefAttr (flip Vty.withBackColor (era_colors !! (1 + (fromIntegral $ abs i) `mod` (length era_colors - 1))))
      Just (Debug.LDVWord {state}) -> case state of
                                        -- Used
                                        True -> modifyDefAttr (flip Vty.withBackColor Vty.green)
                                        -- Unused
                                        False -> id
      _ -> id

prettyCCS :: GenCCSPayload CCSPtr CCPayload -> Text
prettyCCS Debug.CCSPayload{ccsCc = cc} = prettyCC cc

prettyCC :: CCPayload -> Text
prettyCC Debug.CCPayload{..} =
  T.pack ccLabel <> "   " <> T.pack ccMod <> "   " <> T.pack ccLoc

completeClosureDetails :: Debuggee -> (Text, DebugClosure CCSPtr SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr)
                                            -> IO ClosureDetails

completeClosureDetails dbg (label', clos)  =
  getClosureDetails dbg label' . ListFullClosure  =<< fillConstrDesc dbg clos



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


-- Event handling when the main window has focus

handleMain :: Debuggee -> Handler Event OperationalState
handleMain dbg e = do
  os <- get
  case e of
    AppEvent event -> case event of
            PollTick -> return ()
            ProgressMessage t -> do
              put $ footerMessage t os
            ProgressFinished desc runtime ->
              put $ os
                    & running_task .~ Nothing
                    & last_run_time .~ Just (desc, runtime)
                    & footerMode .~ FooterInfo
            AsyncFinished action -> action
    _ | Nothing <- view running_task os ->
      case view keybindingsMode os of
        KeybindingsShown ->
          case e of
            VtyEvent (Vty.EvKey _ _) -> put $ os & keybindingsMode .~ NoOverlay
            _ -> put os
        CommandPicker form cmd_list orig_cmds -> do
          -- Overlapping commands are up/down so handle those just via list, otherwise both
          let handle_form = nestEventM' form (handleFormEvent (() <$ e))
              handle_list =
                case e of
                  VtyEvent vty_e -> nestEventM' cmd_list (handleListEvent vty_e)
                  _ -> return cmd_list
              k form' cmd_list' =
                if (formState form /= formState form') then do
                    let filter_string = formState form'
                        new_elems = Seq.filter (\cmd -> T.toLower filter_string `T.isInfixOf` T.toLower (commandDescription cmd )) orig_cmds
                        cmd_list'' = cmd_list'
                                          & listElementsL .~ new_elems
                                          & listSelectedL .~ if Seq.null new_elems then Nothing else Just 0
                    modify $ keybindingsMode .~ CommandPicker form' cmd_list'' orig_cmds
                  else
                    modify $ keybindingsMode .~ CommandPicker form' cmd_list' orig_cmds


          case e of
              VtyEvent (Vty.EvKey Vty.KUp _) -> do
                list' <- handle_list
                k form list'
              VtyEvent (Vty.EvKey Vty.KDown _) -> do
                list' <- handle_list
                k form list'
              VtyEvent (Vty.EvKey Vty.KEsc _) ->
                put $ os & keybindingsMode .~ NoOverlay
              VtyEvent (Vty.EvKey Vty.KEnter _) -> do
                case listSelectedElement cmd_list of
                  Just (_, cmd)
                    | isCmdDisabled (_version os) cmd ->
                      return () -- If the command is disabled, just ignore the key press
                    | otherwise -> do
                      modify $ keybindingsMode .~ NoOverlay
                      dispatchCommand cmd dbg
                  Nothing  -> return ()
              _ -> do
                form' <- handle_form
                list' <- handle_list
                k form' list'


        NoOverlay -> case view footerMode os of
          FooterInput fm form -> inputFooterHandler dbg fm form (handleMainWindowEvent dbg) (() <$ e)
          _ -> handleMainWindowEvent dbg (() <$ e)
    _ -> return ()

commandPickerMode :: OverlayMode
commandPickerMode =
  CommandPicker
    (newForm [(\w -> forceAttr inputAttr w) @@= editTextField id Overlay (Just 1)] "")
    (list CommandPicker_List commandList 1)
    commandList

savedAndGCRoots :: TreeMode
savedAndGCRoots = SavedAndGCRoots renderClosureDetails

-- ----------------------------------------------------------------------------
-- Commands and Shortcut constants
-- ----------------------------------------------------------------------------

invertFilterEvent :: Vty.Event
invertFilterEvent = Vty.EvKey (KChar 'g') [Vty.MCtrl]

isInvertFilterEvent :: Vty.Event -> Bool
isInvertFilterEvent = (invertFilterEvent ==)

-- All the commands which we support, these show up in keybindings and also the command picker
commandList :: Seq.Seq Command
commandList =
  [ mkCommand  "Show key bindings"            (Vty.EvKey (KChar '?') []) (modify $ keybindingsMode .~ KeybindingsShown)
  , mkCommand  "Clear filters"                     (withCtrlKey 'w') (modify $ clearFilters)
  , Command   "Search with current filters" (Just $ withCtrlKey 'f') searchWithCurrentFilters NoReq
  , mkCommand  "Set search limit (default 100)"    (withCtrlKey 'l') (setFooterInputMode FSetResultSize)
  , mkCommand  "Saved/GC Roots"                    (withCtrlKey 's') (modify $ treeMode .~ savedAndGCRoots)
  , mkCommand  "Find Address"                      (withCtrlKey 'a') (setFooterInputMode (FClosureAddress True False))
  , mkCommand  "Find Info Table"                   (withCtrlKey 't') (setFooterInputMode (FInfoTableAddress True False))
  , mkCommand  "Find Retainers"                    (withCtrlKey 'e') (setFooterInputMode (FConstructorName True False))
  , mkCommand' "Find Retainers (Exact)"                              (setFooterInputMode (FClosureName True False))
  , mkFilterCmd "Find closures by era"             (withCtrlKey 'v') (setFooterInputMode (FFilterEras True False)) ReqErasProfiling
  , mkCommand  "Find Retainers of large ARR_WORDS" (withCtrlKey 'u') (setFooterInputMode FArrWordsSize)
  , mkCommand  "Dump ARR_WORDS payload"            (withCtrlKey 'j') (setFooterInputMode FDumpArrWords)
  , mkCommand  "Write Profile"                     (withCtrlKey 'b') (setFooterInputMode (FProfile OneLevel))
  , mkCommand'  "Write Profile (2 level)"          (setFooterInputMode (FProfile TwoLevel))
  , Command  "Thunk Analysis"                      Nothing thunkAnalysisAction NoReq
  , mkCommand  "Take Snapshot"                     (withCtrlKey 'x') (setFooterInputMode FSnapshot)
  , Command "ARR_WORDS Count" Nothing arrWordsAction NoReq
  , Command "Strings Count" Nothing stringsAction NoReq
  ] <> addFilterCommands
  where
    setFooterInputMode m = modify $ footerMode .~ footerInput m

    addFilterCommands ::  Seq.Seq Command
    addFilterCommands =
      [ mkCommand'   "Add filter for address"             (setFooterInputMode (FClosureAddress False False))
      , mkCommand'   "Add filter for info table ptr"      (setFooterInputMode (FInfoTableAddress False False))
      , mkCommand'   "Add filter for constructor name"    (setFooterInputMode (FConstructorName False False))
      , mkCommand'   "Add filter for closure name"        (setFooterInputMode (FClosureName False False))
      , mkFilterCmd' "Add filter for era"                 (setFooterInputMode (FFilterEras False False)) ReqErasProfiling
      , mkFilterCmd' "Add filter for cost centre id"      (setFooterInputMode (FFilterCcId False False)) ReqSomeProfiling
      , mkCommand'   "Add filter for closure size"        (setFooterInputMode (FFilterClosureSize False))
      , mkCommand'   "Add filter for closure type"        (setFooterInputMode (FFilterClosureType False))
      , mkCommand'   "Add exclusion for address"          (setFooterInputMode (FClosureAddress False True))
      , mkCommand'   "Add exclusion for info table ptr"   (setFooterInputMode (FInfoTableAddress False True))
      , mkCommand'   "Add exclusion for constructor name" (setFooterInputMode (FConstructorName False True))
      , mkCommand'   "Add exclusion for closure name"     (setFooterInputMode (FClosureName False True))
      , mkFilterCmd' "Add exclusion for era"              (setFooterInputMode (FFilterEras False True)) ReqErasProfiling
      , mkFilterCmd' "Add exclusion for cost centre id"   (setFooterInputMode (FFilterCcId False True)) ReqSomeProfiling
      , mkCommand'   "Add exclusion for closure size"     (setFooterInputMode (FFilterClosureSize True))
      , mkCommand'   "Add exclusion for closure type"     (setFooterInputMode (FFilterClosureType True))
      ]

    withCtrlKey char = Vty.EvKey (KChar char) [Vty.MCtrl]

findCommand :: Vty.Event -> Maybe Command
findCommand event = do
  i <- Seq.findIndexL (\cmd -> commandKey cmd == Just event) commandList
  Seq.lookup i commandList


-- ----------------------------------------------------------------------------
-- Window Management
-- ----------------------------------------------------------------------------

handleMainWindowEvent :: Debuggee
                      -> Handler () OperationalState
handleMainWindowEvent dbg brickEvent = do
      os@(OperationalState _ _ treeMode' _kbMode _footerMode _curRoots rootsTree _ _ _ debuggeeVersion) <- get
      case brickEvent of
        VtyEvent (Vty.EvKey (KChar 'p') [Vty.MCtrl]) ->
          put $ os & keybindingsMode .~ commandPickerMode

        -- A generic event
        VtyEvent event
          | Just cmd <- findCommand event ->
            if isCmdDisabled debuggeeVersion cmd
              then return () -- Command is disabled, don't dispatch the command
              else dispatchCommand cmd dbg

        -- Navigate the tree of closures
        VtyEvent event -> case treeMode' of
          SavedAndGCRoots {} -> do
            newTree <- handleIOTreeEvent event rootsTree
            put (os & treeSavedAndGCRoots .~ newTree)
          Retainer r t -> do
            newTree <- handleIOTreeEvent event t
            put (os & treeMode .~ Retainer r newTree)

          Searched r t -> do
            newTree <- handleIOTreeEvent event t
            put (os & treeMode .~ Searched r newTree)

        _ -> return ()

inputFooterHandler :: Debuggee
                   -> FooterInputMode
                   -> Form Text () Name
                   -> Handler () OperationalState
                   -> Handler () OperationalState
inputFooterHandler dbg m form _k re@(VtyEvent e) =
  case e of
    Vty.EvKey KEsc [] -> modify resetFooter
    Vty.EvKey KEnter [] -> dispatchFooterInput dbg m form
    _
      | isInvertFilterEvent e ->
          let m' = invertInput m in
          modify (footerMode .~ (FooterInput m' (updateFormState (formState form) $ footerInputForm m')))
      | otherwise -> do
          zoom (lens (const form) (\ os form' -> set footerMode (FooterInput m form') os)) (handleFormEvent re)
inputFooterHandler _ _ _ k re = k re

stringsAction :: Debuggee -> EventM n OperationalState ()
stringsAction dbg = do
  outside_os <- get
  -- TODO: Does not honour search limit at all
  asyncAction "Counting strings" outside_os (stringsAnalysis Nothing dbg) $ \res -> do
    os <- get
    let cmp (k, v) = length k * (S.size v)
    let sorted_res = maybe id take (_resultSize os) $ Prelude.reverse [(k, S.toList v ) | (k, v) <- (List.sortBy (comparing (S.size . snd)) (M.toList res))]

        top_closure = [CountLine k (length k) (length v) | (k, v) <- sorted_res]

        g_children d (CountLine b _ _) = do
          let Just cs = M.lookup b res
          cs' <- run dbg $ forM (S.toList cs) $ \c -> do
            c' <- GD.dereferenceClosure c
            return $ ListFullClosure $ Closure c c'
          children' <- traverse (traverse (fillListItem d)) $ zipWith (\n c -> (show @Int n, c)) [0..] cs'
          mapM (\(lbl, child) -> FieldLine <$> getClosureDetails d (pack lbl) child) children'
        g_children d (FieldLine c) = map FieldLine <$> getChildren d c

        renderHeaderPane (CountLine k l n) = vBox
          [ labelled "Count     " $ vLimit 1 $ str (show n)
          , labelled "Size      " $ vLimit 1 $ renderBytes l
          , labelled "Total Size" $ vLimit 1 $ renderBytes (n * l)
          , strWrap (take 100 $ show k)
          ]
        renderHeaderPane (FieldLine c) = renderClosureDetails c

        tree = mkIOTree dbg top_closure g_children renderArrWordsLines id
    put (os & resetFooter
            & treeMode .~ Searched renderHeaderPane tree
        )


data ArrWordsLine k = CountLine k Int Int | FieldLine ClosureDetails



renderArrWordsLines :: Show a => ArrWordsLine a -> [Widget n]
renderArrWordsLines (CountLine k l n) = [strLabel (show n), vSpace, renderBytes l, vSpace, strWrap (take 100 $ show k)]
renderArrWordsLines (FieldLine cd) = renderInlineClosureDesc cd

-- | Render a histogram with n lines which displays the number of elements in each bucket,
-- and how much they contribute to the total size.
histogram :: Int -> [GD.Size] -> Widget Name
histogram boxes m =
  vBox $ map displayLine (bin 0 (map calcPercentage (List.sort m )))
  where
    Size maxSize = maximum m

    calcPercentage (Size tot) =
      (tot, (fromIntegral tot/ fromIntegral maxSize) * 100 :: Double)

    displayLine (l, h, n, tot) =
      str (show l) <+> txt "%-" <+> str (show h) <+> str "%: " <+> str (show n) <+> str " " <+> renderBytes tot

    step = fromIntegral (ceiling @Double @Int (100 / fromIntegral boxes))

    bin _ [] = []
    bin k xs = case now of
                 [] -> bin (k + step) later
                 _ -> (k, k+step, length now, sum (map fst now)) : bin (k + step) later
      where
        (now, later) = span ((<= k + step) . snd) xs

-- | Vertical space used to separate elements on the same line.
--
-- This is standardised for a consistent UI.
vSpace :: Widget n
vSpace = txt "   "

arrWordsAction :: Debuggee -> EventM n OperationalState ()
arrWordsAction dbg = do
  outside_os <- get
  asyncAction "Counting ARR_WORDS" outside_os (arrWordsAnalysis Nothing dbg) $ \res -> do
    os <- get
    let all_res = Prelude.reverse [(k, S.toList v ) | (k, v) <- (List.sortBy (comparing (\(k, v) -> fromIntegral (BS.length k) * S.size v)) (M.toList res))]

        display_res = maybe id take (_resultSize os) all_res

        top_closure = [CountLine k (fromIntegral (BS.length k)) (length v) | (k, v) <- display_res]

        !words_histogram = histogram 8 (concatMap (\(k, bs) -> let sz = BS.length k in replicate (length bs) (Size (fromIntegral sz))) all_res)

        g_children d (CountLine b _ _) = do
          let Just cs = M.lookup b res
          cs' <- run dbg $ forM (S.toList cs) $ \c -> do
            c' <- GD.dereferenceClosure c
            return $ ListFullClosure $ Closure c c'
          children' <- traverse (traverse (fillListItem d)) $ zipWith (\n c -> (show @Int n, c)) [0..] cs'
          mapM (\(lbl, child) -> FieldLine <$> getClosureDetails d (pack lbl) child) children'
        g_children d (FieldLine c) = map FieldLine <$> getChildren d c

        renderHeaderPane (CountLine b l n) = vBox
          [ labelled "Count"      $ vLimit 1 $ str (show n)
          , labelled "Size"       $ vLimit 1 $ renderBytes l
          , labelled "Total Size" $ vLimit 1 $ renderBytes (n * l)
          , strWrap (take 100 $ show b)
          ]
        renderHeaderPane (FieldLine c) = renderClosureDetails c

        renderWithHistogram c = joinBorders (renderHeaderPane c <+>
          (padRight (Pad 1) $ (padLeft Brick.Max $ borderWithLabel (txt "Histogram") $ hLimit 100 $ words_histogram)))

        tree = mkIOTree dbg top_closure g_children renderArrWordsLines id
    put (outside_os & resetFooter
            & treeMode .~ Searched renderWithHistogram tree
        )

data ThunkLine = ThunkLine (Maybe SourceInformation) Count

thunkAnalysisAction :: Debuggee -> EventM n OperationalState ()
thunkAnalysisAction dbg = do
  outside_os <- get
  -- TODO: Does not honour search limit at all
  asyncAction "Counting thunks" outside_os (thunkAnalysis dbg) $ \res -> do
    os <- get
    let top_closure = Prelude.reverse [ ThunkLine k v | (k, v) <- (List.sortBy (comparing (getCount . snd)) (M.toList res))]

        g_children _ (ThunkLine {}) = pure []

        renderHeaderPane (ThunkLine sc c) = vBox $
          maybe [txt "NoLoc"] renderSourceInformation sc
          ++ [ strWrap ("Count: " ++ show (getCount c)) ]

        renderInline (ThunkLine msc (Count c)) =
          [(case msc of
              Just sc -> strLabel (infoPosition sc)
              Nothing -> txtLabel "NoLoc"), txt " ", str (show c) ]


        tree = mkIOTree dbg top_closure g_children renderInline id
    put (os & resetFooter
            & treeMode .~ Searched renderHeaderPane tree
        )


searchWithCurrentFilters :: Debuggee -> EventM n OperationalState ()
searchWithCurrentFilters dbg = do
  outside_os <- get
  let mClosFilter = uiFiltersToFilter (_filters outside_os)
  asyncAction "Searching for closures" outside_os (liftIO $ retainersOf (_resultSize outside_os) mClosFilter Nothing dbg) $ \cps -> do
    os <- get
    let cps' = map (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
    res <- liftIO $ mapM (mapM (completeClosureDetails dbg)) cps'
    let tree = mkRetainerTree dbg res
    put (os & resetFooter
            & treeMode .~ Retainer renderClosureDetails tree
        )

filterOrRun :: Debuggee -> Form Text () Name -> Bool -> (String -> Maybe a) -> (a -> [UIFilter]) -> EventM n OperationalState ()
filterOrRun dbg form doRun parse createFilter =
  filterOrRunM dbg form doRun parse (pure . createFilter)

filterOrRunM :: Debuggee -> Form Text () Name -> Bool -> (String -> Maybe a) -> (a -> EventM n OperationalState [UIFilter]) -> EventM n OperationalState ()
filterOrRunM dbg form doRun parse createFilterM = do
  case parse (T.unpack (formState form)) of
    Just x
      | doRun -> do
        newFilter <- createFilterM x
        modify $ setFilters newFilter
        searchWithCurrentFilters dbg
      | otherwise -> do
        newFilter <- createFilterM x
        modify $ (resetFooter . addFilters newFilter)
    Nothing -> modify resetFooter

data ProfileLine  = ProfileLine GDP.ProfileKey GDP.ProfileKeyArgs CensusStats | ClosureLine ClosureDetails

renderProfileLine :: ProfileLine -> [Widget Name]
renderProfileLine (ClosureLine c) = renderInlineClosureDesc c
renderProfileLine (ProfileLine k kargs c) =
 [txt (GDP.prettyShortProfileKey k <> GDP.prettyShortProfileKeyArgs kargs), txt " ",  showLine c]
  where
    showLine :: CensusStats -> Widget Name
    showLine (CS (Count n) (Size s) (Data.Semigroup.Max (Size mn)) _) =
      hBox
        [ withFontColor totalSizeColor $ str (show s),  vSpace
        , withFontColor countColor $ str (show n),  vSpace
        , withFontColor sizeColor $ str (show mn), vSpace
        , withFontColor avgSizeColor $ str (Numeric.showFFloat @Double (Just 1) (fromIntegral s / fromIntegral n) "")
        ]

    withFontColor color = modifyDefAttr (flip Vty.withForeColor color)

    totalSizeColor = Vty.RGBColor 0x26 0x83 0xDE
    countColor = Vty.RGBColor 0xDE 0x66 0x26
    sizeColor = Vty.RGBColor 0x26 0xDE 0xD7
    avgSizeColor = Vty.RGBColor 0xAB 0x4D 0xE0


-- | What happens when we press enter in footer input mode
dispatchFooterInput :: Debuggee
                    -> FooterInputMode
                    -> Form Text () Name
                    -> EventM n OperationalState ()
dispatchFooterInput dbg (FClosureAddress runf invert) form   = filterOrRun dbg form runf readClosurePtr (pure . UIAddressFilter invert)
dispatchFooterInput dbg (FInfoTableAddress runf invert) form = filterOrRun dbg form runf readInfoTablePtr (pure . UIInfoAddressFilter invert)
dispatchFooterInput dbg (FConstructorName runf invert) form  = filterOrRun dbg form runf Just (pure . UIConstructorFilter invert)
dispatchFooterInput dbg (FClosureName runf invert) form      = filterOrRun dbg form runf Just (pure . UIInfoNameFilter invert)
dispatchFooterInput dbg FArrWordsSize form                  = filterOrRun dbg form True readMaybe (\size -> [UIClosureTypeFilter False Debug.ARR_WORDS, UISizeFilter False size])
dispatchFooterInput dbg (FFilterEras runf invert) form       = filterOrRun dbg form runf (parseEraRange . T.pack) (pure . UIEraFilter invert)
dispatchFooterInput dbg (FFilterClosureSize invert) form = filterOrRun dbg form False readMaybe (pure . UISizeFilter invert)
dispatchFooterInput dbg (FFilterClosureType invert) form = filterOrRun dbg form False readMaybe (pure . UIClosureTypeFilter invert)
dispatchFooterInput dbg (FFilterCcId runf invert) form = filterOrRun dbg form runf readMaybe (pure . UICcId invert)
dispatchFooterInput dbg (FProfile lvl) form = do
   outside_os <- get

   asyncAction "Writing profile" outside_os (profile dbg lvl (T.unpack (formState form))) $ \res -> do
    os <- get
    let top_closure = Prelude.reverse [ProfileLine k kargs v  | ((k, kargs), v) <- (List.sortBy (comparing (cssize . snd)) (M.toList res))]

        total_stats = foldMap snd (M.toList res)

        g_children d (ClosureLine c) = map ClosureLine <$> getChildren d c
        g_children d (ProfileLine _ _ stats) = do
          let cs = getSamples (sample stats)
          cs' <- run dbg $ forM cs $ \c -> do
            c' <- GD.dereferenceClosure c
            return $ ListFullClosure $ Closure c c'
          children' <- traverse (traverse (fillListItem d)) $ zipWith (\n c -> (show @Int n, c)) [0..] cs'
          mapM (\(lbl, child) -> ClosureLine <$> getClosureDetails d (pack lbl) child) children'

        renderHeaderPane (ClosureLine cs) = renderClosureDetails cs
        renderHeaderPane (ProfileLine k args (CS (Count n) (Size s) (Data.Semigroup.Max (Size mn)) _)) = vBox $
          [ txtLabel "Label      " <+> vSpace <+> txt (GDP.prettyShortProfileKey k <> GDP.prettyShortProfileKeyArgs args)
          ]
          <>
          (case k of
            GDP.ProfileConstrDesc desc ->
              [ txtLabel "Package    " <+> vSpace <+> (txt (GDP.pkgsText desc))
              , txtLabel "Module     " <+> vSpace <+> (txt (GDP.modlText desc))
              , txtLabel "Constructor" <+> vSpace <+> (txt (GDP.nameText desc))
              ]
            _ -> []
              )
          <>
          [ txtLabel "Count      " <+> vSpace <+> str (show n)
          , txtLabel "Size       " <+> vSpace <+> renderBytes s
          , txtLabel "Max        " <+> vSpace <+> renderBytes mn
          , txtLabel "Average    " <+> vSpace <+> renderBytes @Double (fromIntegral s / fromIntegral n)
          ]

        renderWithStats l = joinBorders $ renderHeaderPane l <+>
          (padRight (Pad 1) $ (padLeft Brick.Max $ renderHeaderPane (ProfileLine (GDP.ProfileClosureDesc "Total") GDP.NoArgs total_stats)))


        tree :: IOTree ProfileLine Name
        tree = mkIOTree dbg top_closure g_children renderProfileLine id
    put (os & resetFooter
            & treeMode .~ Searched renderWithStats tree
        )
dispatchFooterInput _ FDumpArrWords form = do
   os <- get
   let act node = asyncAction_ "dumping ARR_WORDS payload" os $
        case node of
          Just ClosureDetails{_closure = Closure{_closureSized = Debug.unDCS -> Debug.ArrWordsClosure{bytes, arrWords}}} ->
              BS.writeFile (T.unpack $ formState form) $ arrWordsBS (take (fromIntegral bytes) arrWords)
          _ -> pure ()
   case view treeMode os of
      Retainer _ iotree -> act (ioTreeSelection iotree)
      SavedAndGCRoots _ -> act (ioTreeSelection (view treeSavedAndGCRoots os))
      Searched {} -> put (os & footerMessage "Dump for search mode not implemented yet")
dispatchFooterInput _ FSetResultSize form = do
   outside_os <- get
   asyncAction "setting result size" outside_os (pure ()) $ \() -> do
     os <- get
     case readMaybe $ T.unpack (formState form) of
       Just n
         | n <= 0 -> put (os & resultSize .~ Nothing)
         | otherwise -> put (os & resultSize .~ (Just n))
       Nothing -> pure ()
dispatchFooterInput dbg FSnapshot form = do
   os <- get
   asyncAction_ "Taking snapshot" os $ snapshot dbg (T.unpack (formState form))

asyncAction_ :: Text -> OperationalState -> IO a -> EventM n OperationalState ()
asyncAction_ desc  os action = asyncAction desc os action (\_ -> return ())

asyncAction :: Text -> OperationalState -> IO a -> (a -> EventM Name OperationalState ()) -> EventM n OperationalState ()
asyncAction desc os action final = do
  tid <- (liftIO $ forkIO $ do
    writeBChan eventChan (ProgressMessage desc)
    start <- getCurrentTime
    res <- action
    end <- getCurrentTime
    writeBChan eventChan (AsyncFinished (final res))
    writeBChan eventChan (ProgressFinished desc (end `diffUTCTime` start)))
  put $ os & running_task .~ Just tid
           & resetFooter
  where
    eventChan = view event_chan os



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

  mkIOTree dbg roots lookup_c renderInlineClosureDesc id

resetFooter :: OperationalState -> OperationalState
resetFooter l = (set footerMode FooterInfo l)

footerMessage :: Text -> OperationalState -> OperationalState
footerMessage t l = (set footerMode (FooterMessage t) l)

myAppStartEvent :: EventM Name AppState ()
myAppStartEvent = return ()

myAppAttrMap :: AppState -> AttrMap
myAppAttrMap _appState =
  attrMap (Vty.withStyle (Vty.white `on` Vty.black) Vty.dim)
    [ (menuAttr, Vty.withStyle (Vty.white `on` Vty.blue) Vty.bold)
    , (inputAttr, Vty.black `on` Vty.green)
    , (labelAttr, Vty.withStyle (fg Vty.white) Vty.bold)
    , (highlightAttr, Vty.black `on` Vty.yellow)
    , (treeAttr, fg Vty.red)
    , (disabledMenuAttr, Vty.withStyle (grey `on` Vty.blue) Vty.bold)
    ]

menuAttr :: AttrName
menuAttr = attrName "menu"

inputAttr :: AttrName
inputAttr = attrName "input"

labelAttr :: AttrName
labelAttr = attrName "label"

treeAttr :: AttrName
treeAttr = attrName "tree"

highlightAttr :: AttrName
highlightAttr = attrName "highlighted"

disabledMenuAttr :: AttrName
disabledMenuAttr = attrName "disabledMenu"

txtLabel :: Text -> Widget n
txtLabel = withAttr labelAttr . txt

strLabel :: String -> Widget n
strLabel = withAttr labelAttr . str

highlighted :: Widget n -> Widget n
highlighted = forceAttr highlightAttr

disabledMenuItem :: Widget n -> Widget n
disabledMenuItem = forceAttr disabledMenuAttr

main :: IO ()
main = do
  eventChan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan eventChan PollTick
    -- 2s
    threadDelay 2_000_000
  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  let app :: App AppState Event Name
      app = App
        { appDraw = myAppDraw
        , appChooseCursor = showFirstCursor
        , appHandleEvent = myAppHandleEvent
        , appStartEvent = myAppStartEvent
        , appAttrMap = myAppAttrMap
        }
  _finalState <- customMain initialVty buildVty
                    (Just eventChan) app (initialAppState eventChan)
  return ()
