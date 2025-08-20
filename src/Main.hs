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
import Lucid
import qualified Control.Exception as E
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.GraphViz
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Generalised (DotGraph)
import Debug.Trace

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

-- STATUS: Done
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

-- STATUS: Design
mainBorder :: Text -> Widget a -> Widget a
mainBorder title w = -- borderWithLabel (txt title) . padAll 1
  vLimit 1 (withAttr menuAttr $ hCenter $ fill ' ' <+> txt title <+> fill ' ') <=> w

-- STATUS: Incomplete
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

-- STATUS: Done
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

-- STATUS: Done
renderSourceInformation :: SourceInformation -> [Widget Name]
renderSourceInformation (SourceInformation name cty ty label' modu loc) =
    [ labelled "Name" $ vLimit 1 (str name)
    , labelled "Closure type" $ vLimit 1 (str (show cty))
    , labelled "Type" $ vLimit 3 (str ty)
    , labelled "Label" $ vLimit 1 (str label')
    , labelled "Module" $ vLimit 1 (str modu)
    , labelled "Location" $ vLimit 1 (str loc)
    ]

-- STATUS: Design
labelled :: Text -> Widget Name -> Widget Name
labelled = labelled' 20

-- STATUS: Design
labelled' :: Int -> Text -> Widget Name -> Widget Name
labelled' leftSize lbl w =
  hLimit leftSize  (txtLabel lbl <+> vLimit 1 (fill ' ')) <+> w <+> vLimit 1 (fill ' ')

-- STATUS: Incomplete (DONE for most cases)
renderUIFilter :: UIFilter -> Widget Name
renderUIFilter (UIAddressFilter invert x)     = labelled (bool "" "!" invert <> "Closure address") (str (show x))
renderUIFilter (UIInfoAddressFilter invert x) = labelled (bool "" "!" invert <> "Info table address") (str (show x))
renderUIFilter (UIConstructorFilter invert x) = labelled (bool "" "!" invert <> "Constructor name") (str x)
renderUIFilter (UIInfoNameFilter invert x)    = labelled (bool "" "!" invert <> "Constructor name (exact)") (str x)
renderUIFilter (UIEraFilter invert  x)        = labelled (bool "" "!" invert <> "Era range") (str (showEraRange x))
renderUIFilter (UISizeFilter invert x)        = labelled (bool "" "!" invert <> "Size (lower bound)") (str (show $ getSize x))
renderUIFilter (UIClosureTypeFilter invert x) = labelled (bool "" "!" invert <> "Closure type") (str (show x))
renderUIFilter (UICcId invert x)              = labelled (bool "" "!" invert <> "CC Id") (str (show x))

-- STATUS: Done
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

-- STATUS: Done
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

-- STATUS: Done
renderBytes :: Real a => a -> Widget n
renderBytes n =
  str (getShortHand (getAppropriateUnits (ByteValue (realToFrac n) Bytes)))

-- STATUS: Design
footer :: Int -> Maybe Int -> FooterMode -> Widget Name
footer n m fmode = vLimit 1 $
 case fmode of
   FooterMessage t -> withAttr menuAttr $ hBox [txt t, fill ' ']
   FooterInfo -> withAttr menuAttr $ hBox $ [padRight Brick.Max $ txt "(↑↓): select item | (→): expand | (←): collapse | (^p): command picker | (^g): invert filter | (?): full keybindings"]
                                         ++ [padLeft (Pad 1) $ str $
                                               (show n <> " items/" <> maybe "∞" show m <> " max")]
   FooterInput _im form -> renderForm form

-- STATUS: Design
footerInput :: FooterInputMode -> FooterMode
footerInput im =
  FooterInput im (footerInputForm im)

-- STATUS: Design
footerInputForm :: FooterInputMode -> Form Text e Name
footerInputForm im =
  newForm [(\w -> txtLabel (formatFooterMode im) <+> forceAttr inputAttr w) @@= editTextField id Footer (Just 1)] ""

-- STATUS: Done (in use)
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

-- STATUS: Design
era_colors :: [Vty.Color]
era_colors = [Vty.Color240 n | n <- [17..230]]

-- STATUS: Design
grey :: Vty.Color
grey = Vty.rgbColor (158 :: Int) 158 158

-- STATUS: Design
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

-- STATUS: Design (unsure)
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

-- STATUS: Done
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

-- STATUS: Done
prettyCCS :: GenCCSPayload CCSPtr CCPayload -> Text
prettyCCS Debug.CCSPayload{ccsCc = cc} = prettyCC cc

-- STATUS: Done
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

-- STATUS: Incomplete (unsure)
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


-- STATUS: Incomplete
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

-- STATUS: Incomplete
commandPickerMode :: OverlayMode
commandPickerMode =
  CommandPicker
    (newForm [(\w -> forceAttr inputAttr w) @@= editTextField id Overlay (Just 1)] "")
    (list CommandPicker_List commandList 1)
    commandList

-- STATUS: Done (unsure, in use)
savedAndGCRoots :: TreeMode
savedAndGCRoots = SavedAndGCRoots renderClosureDetails

-- ----------------------------------------------------------------------------
-- Commands and Shortcut constants
-- ----------------------------------------------------------------------------

-- STATUS: Incomplete
invertFilterEvent :: Vty.Event
invertFilterEvent = Vty.EvKey (KChar 'g') [Vty.MCtrl]

-- STATUS: Incomplete
isInvertFilterEvent :: Vty.Event -> Bool
isInvertFilterEvent = (invertFilterEvent ==)

-- STATUS: Incomplete
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

-- STATUS: Incomplete
findCommand :: Vty.Event -> Maybe Command
findCommand event = do
  i <- Seq.findIndexL (\cmd -> commandKey cmd == Just event) commandList
  Seq.lookup i commandList


-- ----------------------------------------------------------------------------
-- Window Management
-- ----------------------------------------------------------------------------

-- STATUS: Incomplete
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

-- STATUS: Design
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

-- STATUS: Done
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


-- STATUS: Done
renderArrWordsLines :: Show a => ArrWordsLine a -> [Widget n]
renderArrWordsLines (CountLine k l n) = [strLabel (show n), vSpace, renderBytes l, vSpace, strWrap (take 100 $ show k)]
renderArrWordsLines (FieldLine cd) = renderInlineClosureDesc cd

-- STATUS: Done
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

-- STATUS: Design
-- | Vertical space used to separate elements on the same line.
--
-- This is standardised for a consistent UI.
vSpace :: Widget n
vSpace = txt "   "

-- STATUS: Done
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

-- STATUS: Done
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


-- STATUS: Done
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

-- STATUS: Done (kind of)
filterOrRun :: Debuggee -> Form Text () Name -> Bool -> (String -> Maybe a) -> (a -> [UIFilter]) -> EventM n OperationalState ()
filterOrRun dbg form doRun parse createFilter =
  filterOrRunM dbg form doRun parse (pure . createFilter)

-- STATUS: Done (kind of)
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

-- STATUS: Done
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


-- STATUS: Incomplete
-- | What happens when we press enter in footer input mode
dispatchFooterInput :: Debuggee
                    -> FooterInputMode
                    -> Form Text () Name
                    -> EventM n OperationalState ()
-- DONE
dispatchFooterInput dbg (FClosureAddress runf invert) form   = filterOrRun dbg form runf readClosurePtr (pure . UIAddressFilter invert)
-- DONE
dispatchFooterInput dbg (FInfoTableAddress runf invert) form = filterOrRun dbg form runf readInfoTablePtr (pure . UIInfoAddressFilter invert)
-- DONE
dispatchFooterInput dbg (FConstructorName runf invert) form  = filterOrRun dbg form runf Just (pure . UIConstructorFilter invert)
-- DONE
dispatchFooterInput dbg (FClosureName runf invert) form      = filterOrRun dbg form runf Just (pure . UIInfoNameFilter invert)
-- Incomplete
dispatchFooterInput dbg FArrWordsSize form                  = filterOrRun dbg form True readMaybe (\size -> [UIClosureTypeFilter False Debug.ARR_WORDS, UISizeFilter False size])
-- Incomplete
dispatchFooterInput dbg (FFilterEras runf invert) form       = filterOrRun dbg form runf (parseEraRange . T.pack) (pure . UIEraFilter invert)
-- DONE
dispatchFooterInput dbg (FFilterClosureSize invert) form = filterOrRun dbg form False readMaybe (pure . UISizeFilter invert)
-- DONE
dispatchFooterInput dbg (FFilterClosureType invert) form = filterOrRun dbg form False readMaybe (pure . UIClosureTypeFilter invert)
-- Incomplete
dispatchFooterInput dbg (FFilterCcId runf invert) form = filterOrRun dbg form runf readMaybe (pure . UICcId invert)
-- DONE
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
-- DONE (although consider making this accessible from profile page etc.)
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
-- Incomplete
dispatchFooterInput _ FSetResultSize form = do
   outside_os <- get
   asyncAction "setting result size" outside_os (pure ()) $ \() -> do
     os <- get
     case readMaybe $ T.unpack (formState form) of
       Just n
         | n <= 0 -> put (os & resultSize .~ Nothing)
         | otherwise -> put (os & resultSize .~ (Just n))
       Nothing -> pure ()
-- DONE
dispatchFooterInput dbg FSnapshot form = do
   os <- get
   asyncAction_ "Taking snapshot" os $ snapshot dbg (T.unpack (formState form))

-- STATUS: Design
asyncAction_ :: Text -> OperationalState -> IO a -> EventM n OperationalState ()
asyncAction_ desc  os action = asyncAction desc os action (\_ -> return ())

-- STATUS: Design
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


-- STATUS: Done
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

-- STATUS: Design
resetFooter :: OperationalState -> OperationalState
resetFooter l = (set footerMode FooterInfo l)

-- STATUS: Design
footerMessage :: Text -> OperationalState -> OperationalState
footerMessage t l = (set footerMode (FooterMessage t) l)

-- STATUS: Design
myAppStartEvent :: EventM Name AppState ()
myAppStartEvent = return ()

-- STATUS: Design
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

-- STATUS: Design
menuAttr :: AttrName
menuAttr = attrName "menu"

-- STATUS: Design
inputAttr :: AttrName
inputAttr = attrName "input"

-- STATUS: Design
labelAttr :: AttrName
labelAttr = attrName "label"

-- STATUS: Design
treeAttr :: AttrName
treeAttr = attrName "tree"

-- STATUS: Design
highlightAttr :: AttrName
highlightAttr = attrName "highlighted"

-- STATUS: Design
disabledMenuAttr :: AttrName
disabledMenuAttr = attrName "disabledMenu"

-- STATUS: Design
txtLabel :: Text -> Widget n
txtLabel = withAttr labelAttr . txt

-- STATUS: Design
strLabel :: String -> Widget n
strLabel = withAttr labelAttr . str

-- STATUS: Design
highlighted :: Widget n -> Widget n
highlighted = forceAttr highlightAttr

-- STATUS: Design
disabledMenuItem :: Widget n -> Widget n
disabledMenuItem = forceAttr disabledMenuAttr

-- STATUS: Incomplete
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

renderConnectedPage :: [Int] -> Maybe (Int, Bool) -> SocketInfo -> Debuggee -> ConnectedMode -> TL.Text
renderConnectedPage selectedPath mInc socket debuggee mode = renderText $ case mode of
  RunningMode -> do
    h2_ "Status: running mode. There is nothing you can do until you pause the process."
    form_ [method_ "post", action_ "/pause"] $ 
      button_ "Pause process" 
  PausedMode os -> do
    let tree = _treeSavedAndGCRoots os

    h2_ $ toHtml ("ghc-debug - Paused " <> socketName socket)
    form_ [method_ "post", action_ "/resume"] $
      button_ "Resume process"
    form_ [method_ "post", action_ "/exit"] $
      button_ "Exit"

    div_ [ style_ "display: flex; gap: 2rem; align-items: flex-start; margin-bottom: 0.25rem;" ] $ do
      -- Left column: Summary and stop/start buttons
      div_ [ style_ "flex: 1; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;" ] $ do
        detailedSummary renderClosureSummary tree selectedPath mInc
        

      -- Right column: Analysis buttons
      div_ [ style_ "flex: 1;" ] $ do
        div_ [ style_ "margin: 0; display: flex; flex-direction: column; gap: 0.25rem;" ] $ do
          --div_ [style_ "position: absolute; top: 50px; right: 10px;"] $ do
          form_ [ method_ "post", action_ "/profile"
                , style_ "margin: 0; display: flex; align-items: center; gap: 8px;"] $ do
            button_ [type_ "submit"] (toHtml ("View profile" :: Text))
            select_ [name_ "profileLevel"] $ do
              option_ [value_ "1"] (toHtml ("Level one" :: Text))
              option_ [value_ "2"] (toHtml ("Level two" :: Text))
          form_ [ method_ "post", action_ "/arrWordsCount"
                , style_ "margin: 0; display: flex; align-items: center; gap: 8px;"] $ do
            button_ [type_ "submit"] "View ARR_WORDS count"
          form_ [ method_ "post", action_ "/stringsCount"
                , style_ "margin: 0; display: flex; align-items: center; gap: 8px;"] $ do
            button_ [type_ "submit"] "View strings count"
          form_ [ method_ "post", action_ "/thunkAnalysis"
                , style_ "margin: 0; display: flex; align-items: center; gap: 8px;"] $ do
            button_ [type_ "submit"] "View thunk analysis"
          form_ [ method_ "post", action_ "/takeSnapshot"
                , style_ "margin: 0; display: flex; align-items: center; gap: 8px;"] $ do
            input_ [type_ "text", name_ "filename", placeholder_ "Enter snapshot filename", required_ "required"]

            input_ [type_ "hidden", name_ "selected", value_ (encodePath selectedPath)]
            button_ [type_ "submit"] "Take snapshot"
          form_ [ method_ "post", action_ "/searchWithFilters"
                , style_ "margin: 0; display: flex; align-items: center; gap: 8px;"] $ do
            button_ [type_ "submit"] "Search with current filters"

    h3_ $ toHtml $ case os ^. treeMode of
      SavedAndGCRoots {} -> pack "Root Closures"
      Retainer {} -> pack "Retainers"
      Searched {} -> pack "Search Results"
    renderIOTreeHtml tree selectedPath (detailedRowHtml renderClosureHtml "connect")
    autoScrollScript

renderClosureSummary :: ClosureDetails -> [Int] -> Maybe (Int, Bool) -> Html ()
renderClosureSummary node path mInc =
  case node of
    ClosureDetails c excSize info -> do 
      renderInfoSummary info
      li_ $ do
        strong_ "Exclusive size: "
        toHtml (show (getSize excSize) <> "B")
        case node of 
          ClosureDetails{_closure = Closure{_closureSized = Debug.unDCS -> Debug.ArrWordsClosure{bytes, arrWords}}} -> do
            li_ $ a_ [href_ ("/dumpArrWords?selected=" <> encodePath path)] "Dump ARR_WORDS payload"
          _ -> mempty
        case mInc of
          Nothing -> mempty
          Just (incSize, capped) -> renderIncSize incSize capped path
        form_ [method_ "post", action_ "/img"] $ do
          input_ [type_ "hidden", name_ "selected", value_ (encodePath path)]
          button_ [type_ "submit", class_ "viz-button"] $ "See graph" 

    LabelNode n -> li_ $ toHtml n
    InfoDetails info -> renderInfoSummary info
    CCSDetails _ _ptr (Debug.CCSPayload{..}) -> do
      li_ $ do 
        strong_ "ID: "
        toHtml (show ccsID) 
      renderCC ccsCc
    CCDetails _ c -> renderCC c

detailedSummary :: (Ord name, Show name)
                => (a -> [Int] -> Maybe (Int, Bool) -> Html ())
                -> IOTree a name -> [Int] -> Maybe (Int, Bool) -> Html ()
detailedSummary f tree@(IOTree _ roots _ _ _) path mInc =
  div_ [class_ "selection-summary"] $ do
    if null roots 
      then mempty
      else do 
        let (IOTreeNode node _) = getSubTree tree path
        f node path mInc

summaryEntry :: (Monad m, Term (HtmlT m ()) result, ToHtml a) => HtmlT m () -> a -> result
summaryEntry title value = li_ $ strong_ (title <> ": ") >> toHtml value

renderProfileSummary :: CensusStats -> ProfileLine -> [Int] -> Maybe (Int, Bool) -> Html ()
renderProfileSummary totalStats line path mInc = do
  div_ [ style_ "display: flex; gap: 2rem; align-items: flex-start;" ] $ do
    -- Left column: Line summary
    div_ [ style_ "flex: 1; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;" ] $ do
      h3_ "Selection: "
      ul_ $ renderLineSummary line

    -- Right column: Total stats
    div_ [ style_ "flex: 1;" ] $ do
      h3_ "Total: "
      ul_ $ renderLineSummary (ProfileLine (GDP.ProfileClosureDesc "Total") GDP.NoArgs totalStats)
  where 
    renderLineSummary (ClosureLine cs) = renderClosureSummary cs path mInc
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

  
renderCountSummary :: Show a => Maybe (Html ()) -> ArrWordsLine a -> [Int] -> Maybe (Int, Bool) -> Html ()
renderCountSummary mh line path mInc = do
  div_ [ style_ "display: flex; gap: 2rem; align-items: flex-start;" ] $ do
    -- Left column: Line summary
    div_ [ style_ "flex: 1; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;" ] $ do
      h3_ "Selection: "
      ul_ $ renderLineSummary line

    -- Right column: Histogram
    case mh of
      Just histo -> 
        div_ [ style_ "flex: 1;" ] $ do
          h3_ "Histogram: "
          ul_ $ histo
      Nothing -> mempty

    where
      renderLineSummary :: Show a => ArrWordsLine a -> Html ()
      renderLineSummary (CountLine b l n) = do
        summaryEntry "Count" (show n)
        summaryEntry "Size" (renderBytesHtml l)
        summaryEntry "Total size" (renderBytesHtml $ n * l)
        li_ $ toHtml $ trunc (show b)
      renderLineSummary (FieldLine c) = renderClosureSummary c path mInc

renderThunkAnalysisSummary :: ThunkLine -> [Int] -> Maybe (Int, Bool) -> Html ()
renderThunkAnalysisSummary (ThunkLine msc c) _ _ = do
  h3_ "Selection: "
  case msc of
    Nothing -> toHtml ("NoLoc" :: Text)
    Just sc -> renderSourceInfoSummary sc
  summaryEntry "Count" (show $ getCount c)

renderIncSize :: Int -> Bool -> [Int] -> Html ()
renderIncSize incSize capped selectedPath = do 
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

renderImgPage :: String -> String -> [Int] -> Bool -> TL.Text
renderImgPage returnTo name selectedPath capped =
  renderText $ do
    h1_ $ toHtml $ "Visualisation of " ++ name
    if capped then h2_ $ "Note: this is a very large object, and this tree is incomplete" else mempty
    body_ $ do
      let pathStr = encodePath selectedPath
      div_ $ a_ [href_ ("/" <> pack returnTo <> "?selected=" <> pathStr)] $ "Return to debuggee"
      div_ $ a_ [ href_ "/graph"
                , download_ "graph.svg"
                , style_ "display: inline-block; margin-top: 1em;"
                ] "Download SVG"
      img_ [src_ "/graph", alt_ "Dynamic Graph", style_ "max-width: 100%; height: auto;"]


reconnectLink = do
  div_ $ form_ [method_ "post", action_ "/reconnect", style_ "display:inline"] $
    button_ [ type_ "submit"
            , style_ "background:none; border:none; padding:0; color:blue; text-decoration:underline; cursor:pointer; font:inherit" 
            ] "Return to saved objects and GC roots"


genericTreeBody tree selectedPath renderRow renderSummary' name mInc = do
  detailedSummary renderSummary' tree selectedPath mInc
  h3_ "Results"
  renderIOTreeHtml tree selectedPath (detailedRowHtml renderRow name)
  autoScrollScript

renderProfilePage :: [Int] -> Maybe (Int, Bool) -> ConnectedMode -> TL.Text
renderProfilePage selectedPath mInc mode = renderText $ case mode of
  PausedMode os -> do
    case _treeMode os of
      SearchedHtml Utils{..} tree name -> do
        h1_ "Profile"
        reconnectLink
        div_ $ a_ [href_ "/download-profile", download_ "profile_dump", style_ "display: inline-block; margin-top: 1em;" ] "Download"
        genericTreeBody tree selectedPath _renderRow _renderSummary name mInc

renderCountPage :: String -> [Int] -> Maybe (Int, Bool) -> ConnectedMode -> TL.Text
renderCountPage title selectedPath mInc mode = renderText $ case mode of
  PausedMode os -> do
    case _treeMode os of
      SearchedHtml Utils{..} tree name -> do
        h1_ $ toHtml $ title ++ " Count"
        reconnectLink
        genericTreeBody tree selectedPath _renderRow _renderSummary name mInc
        
renderThunkAnalysisPage :: [Int] -> Maybe (Int, Bool) -> ConnectedMode -> TL.Text
renderThunkAnalysisPage selectedPath mInc mode = renderText $ case mode of
  PausedMode os -> do
    case _treeMode os of
      SearchedHtml Utils{..} tree name -> do
        h1_ "Thunk analysis"
        reconnectLink
        genericTreeBody tree selectedPath _renderRow _renderSummary name mInc

renderFilterSearchPage :: [Int] -> Maybe (Int, Bool) -> ConnectedMode -> TL.Text
renderFilterSearchPage selectedPath mInc mode = renderText $ case mode of
  PausedMode os -> do
    case _treeMode os of
      Retainer _ tree -> do
        h1_ "Results for search with filters"
        reconnectLink
        div_ [ style_ "display: flex; gap: 2rem; align-items: flex-start;" ] $ do
          -- Left column: Line summary
          div_ [ style_ "flex: 1; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;" ] $ do
            h3_ "Selection: "
            ul_ $ detailedSummary renderClosureSummary tree selectedPath mInc
        
          -- Right column: List of filters
          div_ [ style_ "flex: 1;" ] $ do
            form_ [ method_ "post", action_ "/modifyFilters"
                  , style_ "margin: 0; display: flex; align-items: center; gap: 8px;"] $ do
              h3_ "Filters: "
              button_ "Modify filters"
            ul_ [style_ "margin-top: 0.25rem;"] $ plainUIFilters (_filters os)

        renderIOTreeHtml tree selectedPath (detailedRowHtml renderClosureHtml "searchWithFilters")
        autoScrollScript

renderModifyFilterPage :: ConnectedMode -> TL.Text
renderModifyFilterPage mode = renderText $ case mode of
  PausedMode os -> do
    h1_ "Modify filters"
     
    div_ $ form_ [method_ "post", action_ "/searchWithFilters", style_ "display:inline"] $
      button_ [ type_ "submit"
              , style_ "background:none; border:none; padding:0; color:blue; text-decoration:underline; cursor:pointer; font:inherit" 
              ] "Return to search page"


    div_ [ style_ "display: flex; gap: 2rem; align-items: flex-start;" ] $ do
      -- Left column: List of filters
      div_ [ style_ "flex: 1; word-wrap: break-word; overflow-wrap: break-word; white-space: normal;" ] $ do
        h3_ "Current filters: "
        ul_ $ mapM_ (uncurry renderUIFilterHtml) (zip (_filters os) [0..])
      
      -- Right column: Buttons to modify filters
      div_ [ style_ "flex: 1;" ] $ do
        genFilterButtons "Enter closure address" "Address" 
        genFilterButtons "Enter info table address" "InfoAddress" 
        genFilterButtons "Enter constructor name" "ConstrName"
        genFilterButtons "Enter closure name" "ClosureName"
        genFilterButtons "Enter closure size (B)" "ClosureSize"
        genFilterButtons "Enter closure type" "ClosureType"
  

genFilterButtons :: String -> String -> Html ()
genFilterButtons flavourText filterType = do
 form_ [ method_ "post", action_ "/addFilter"
       , style_ "margin: 0; display: flex; align-items: center; gap: 8px;"] $ do
     input_ [type_ "text", name_ "pattern", placeholder_ (pack flavourText), required_ "required"]
     input_ [type_ "hidden", name_ "filterType", value_ (pack filterType)]
     button_ [type_ "submit", name_ "invert", value_ "False"] "Add filter"
     button_ [type_ "submit", name_ "invert", value_ "True"] "Exclude"


        
        
detailedRowHtml :: (a -> Html ()) -> String -> [Int] -> [Int] -> Bool -> Bool -> a -> Html ()
detailedRowHtml renderHtml name selectedPath thisPath expanded selected obj =
  let depth = length thisPath
      indentPx = depth * 20
      classStr = "tree-row" <> if selected then " selected" else ""
      styleAttr = style_ $ pack ("margin-left: " <> show indentPx <> "px; display: flex; align-items: center; gap: 4px;")
      pathStr = encodePath thisPath
      selectedStr = encodePath selectedPath
      linkStyle = "color: " ++ (if selected then "purple" else "blue") ++ "; text-decoration: none;"
  in div_ [class_ classStr, styleAttr] $ do
       form_ [method_ "post", action_ "/toggle", style_ "margin: 0;"] $ do
         input_ [type_ "hidden", name_ "toggle", value_ pathStr]
         input_ [type_ "hidden", name_ "selected", value_ selectedStr]
         button_ [type_ "submit", class_ "expand-button"] $
           toHtml $ if expanded then "+" else "-" :: String
       a_ [href_ ("/" <> pack name <> "?selected=" <> pathStr), style_ (pack $ linkStyle)] $ renderHtml obj

renderClosureHtml :: ClosureDetails -> Html ()
renderClosureHtml (ClosureDetails closure _excSize info) = div_ [class_ "closure-row"] $ do
  li_ $ toHtml $ _labelInParent info <> " | " <> pack (closureShowAddress closure) <> " | " <> _pretty info 
renderClosureHtml (InfoDetails info) = div_ [class_ "closure-row"] $ do
  li_ $ toHtml $ _labelInParent info <> " | " <> _pretty info 
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
  summaryEntry "Label" label'
  summaryEntry "Module" modu
  summaryEntry "Location" loc


renderInfoSummary :: InfoInfo -> Html ()
renderInfoSummary info = do 
  maybe mempty renderSourceInfoSummary (_sourceLocation info)
  case _profHeaderInfo info of
    Just x ->
      let label = case x of 
                    Debug.RetainerHeader{} -> "Retainer info: "
                    Debug.LDVWord{} -> "LDV info: "
                    Debug.EraWord{} -> "Era: "
                    Debug.OtherHeader{} -> "Other: "
      in summaryEntry label (renderProfHeader x)
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


getIncSize :: (a -> Maybe String) -> (a -> Int) -> IOTree a name -> [Int] -> IO (Int, Bool)
getIncSize getName getSize' tree@(IOTree _ roots _ _ _) selectedPath = do
  if null roots 
    then return (0, False)
    else do 
      let subtree = getSubTree tree selectedPath
      (expSubTree, capped) <- liftIO $ expandNodeSafe subtree (maybe "" id . getName)
      return (getClosureIncSize getName getSize' Set.empty expSubTree, capped)


getClosureIncSize :: (a -> Maybe String) -> (a -> Int) -> Set.Set String -> IOTreeNode a name -> Int
getClosureIncSize getName getSize' seen' node = fst (go seen' node)
  where
    go seen (IOTreeNode n csE) =
      case getName n of
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

type EdgeList = [(String, String)]

getClosureVizTree :: (a -> String) -> Set.Set String -> EdgeList -> IOTreeNode a name -> (Set.Set String, EdgeList)
getClosureVizTree format' nodes edges (IOTreeNode n csE) = 
  let ptr = format' n
  in if Set.member ptr nodes
     then (nodes, [])
     else case csE of
            Left _ -> (Set.insert ptr nodes, [])
            Right csE -> 
              let nodes'' = Set.insert ptr nodes
                  (nodesFinal, childEdges) = listApply (getClosureVizTree format') (nodes'', edges) csE
                  children = [ format' n'
                             | IOTreeNode n' _ <- csE ]
                  newEdges = map (\ch -> (ptr, ch)) children
              in (nodesFinal, childEdges ++ newEdges)
  where
    listApply f (ns, es) xs =
      foldl (\(nsAcc, esAcc) x ->
               let (ns', es') = f nsAcc [] x
               in (ns', esAcc ++ es')) (ns, es) xs
getClosureVizTree _ nodes edges _ = (nodes, edges)

renderBytesHtml :: Real a => a -> String
renderBytesHtml n = getShortHand (getAppropriateUnits (ByteValue (realToFrac n) Bytes))

renderUIFilterHtml :: UIFilter -> Int -> Html ()
renderUIFilterHtml (UIAddressFilter inv x) = renderUIFLine inv "Closure address" show x 
renderUIFilterHtml (UIInfoAddressFilter inv x) = renderUIFLine inv "Info table address" show x
renderUIFilterHtml (UIConstructorFilter inv x) = renderUIFLine inv "Constructor name" id x
renderUIFilterHtml (UIInfoNameFilter inv x) = renderUIFLine inv "Constructor name (exact)" id x
renderUIFilterHtml (UISizeFilter inv x) = renderUIFLine inv "Size (lower bound)" (show . getSize) x
renderUIFilterHtml (UIClosureTypeFilter inv x) = renderUIFLine inv "Closure type" show x
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
closureFormat (ClosureDetails clo _ inf) = closureShowAddress clo ++ "\n" ++ takeWhile (/=' ') (unquote (show (_pretty inf)))
closureFormat (InfoDetails inf) = unquote (show (_labelInParent inf))
closureFormat (LabelNode l) = unquote $ show l
closureFormat x = error $ "viztree format, missing implementation: " ++ show x

getNodeName :: IOTreeNode ClosureDetails name -> String
getNodeName (IOTreeNode (ClosureDetails c excSize info) _) = closureShowAddress c
getNodeName (IOTreeNode (InfoDetails info) _) = show $ _labelInParent info

parsePath :: String -> [Int]
parsePath [] = []
parsePath s = map read $ splitOn "." s

parseProfileLevel :: String -> ProfileLevel
parseProfileLevel "1" = OneLevel
parseProfileLevel "2" = TwoLevel  

unquote :: String -> String
unquote ('\"':xs) | last xs == '\"' = init xs
unquote xs = xs

trunc :: String -> String
trunc s = take n s ++ (if length s > n then "..." else "")
  where n = 30
truncT :: Text -> Text
truncT = pack . trunc . T.unpack

encodePath :: [Int] -> Text
encodePath = pack . List.intercalate "." . map show

togglePath :: Eq a => [a] -> [[a]] -> [[a]]
togglePath x xs = if x `elem` xs then filter (/=x) xs else x:xs

toggleSelected :: Eq a => [a] -> [a] -> [a]
toggleSelected selectedPath togglePath
  | sLen <= tLen = selectedPath
  | take tLen selectedPath == togglePath = togglePath  
  | otherwise = selectedPath
  where sLen = length selectedPath
        tLen = length togglePath

-- Helper function for reading parameters
-- name is a String, the name of the parameter
-- getParam is Scotty.[path|query|form]Param
-- f 'parses' the raw parameter
-- def is the default value
readParam name getParam f def = do
  p <- getParam name `Scotty.rescue` (\ (_ :: E.SomeException) -> return def)
  return $ f p

selectedParam getParam = readParam "selected" getParam parsePath "0"
togglePathParam getParam = readParam "toggle" getParam parsePath ""
profileLevelParam getParam = readParam "profileLevel" getParam parseProfileLevel "1" 
filterTypeParam getParam = readParam "filterType" getParam id ""
patternParam getParam = readParam "pattern" getParam id ""
invertParam getParam = readParam "invert" getParam (read :: String -> Bool) "False"
indexParam getParam = readParam "index" getParam (read :: String -> Int) "(-1)"

buildClosureGraph :: [String] -> EdgeList -> Data.GraphViz.Types.Generalised.DotGraph Int
buildClosureGraph nodes edges = digraph (Str "Visualisation") $ do
  -- possible style for source node, except this logic doesn't always select the source node
  --node sid [toLabel (pack sn :: Text), Data.GraphViz.style filled, fillColor Yellow, color Red]
  mapM_ (\(n, nid) -> node nid [toLabel (pack n :: Text)]) nids
  mapM_ (\(a, b) -> case (lookup a nids, lookup b nids) of
                      (Just x, Just y) -> edge x y []
                      z -> error ("Error in building closure graph: " ++ 
                                  "Arg a: " ++ a ++ ", Arg b: " ++ b ++ " -> " ++ (show z) ++ " -- nids : " ++ show nids)) edges
  where nids@((sn, sid):rest) = zipWith (\n i -> (n,i)) nodes [1..]

histogramHtml :: Int -> [GD.Size] -> Html ()
histogramHtml boxes m = do
  mapM_ displayLine (bin 0 (map calcPercentage (List.sort m )))
  where
    Size maxSize = maximum m
    calcPercentage (Size tot) = (tot, (fromIntegral tot/ fromIntegral maxSize) * 100 :: Double)
    displayLine (l, h, n, tot) =
      li_ $ toHtml $ show l <> "%-" <> show h <> "%: " <> show n <> " " <> renderBytesHtml tot
    step = fromIntegral (ceiling @Double @Int (100 / fromIntegral boxes))
    bin _ [] = []
    bin k xs = case now of
                 [] -> bin (k + step) later
                 _ -> (k, k+step, length now, sum (map fst now)) : bin (k + step) later
      where
        (now, later) = span ((<= k + step) . snd) xs

autoScrollScript = script_ $ mconcat
                     [ "window.addEventListener('beforeunload', () => {"
                     , "sessionStorage.setItem('scrollY', window.scrollY);"
                     , "});"
                     , "window.addEventListener('load', () => {"
                     , "const scrollY = sessionStorage.getItem('scrollY');"
                     , "if (scrollY !== null) window.scrollTo(0, parseInt(scrollY, 10));"
                     , "});"
                     ]

svgPath :: String
svgPath = "tmp/graph.svg"

genericGet :: IORef AppState
           -> Scotty.RoutePattern
           -> ([Int] -> Maybe (Int, Bool) -> ConnectedMode -> TL.Text)
           -> Scotty.ScottyM ()
genericGet appStateRef index renderPage = do
  Scotty.get index $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected socket debuggee mode ->
        case mode of 
          PausedMode os -> do 
            case _treeMode os of 
               SearchedHtml Utils{..} tree _ -> do
                 (incSize, capped) <- liftIO $ getIncSize _getName _getSize tree selectedPath
                 Scotty.html $ renderPage selectedPath (Just (incSize, capped)) mode

dumpArrWord cs = do
  case cs of
    ClosureDetails{_closure = Closure{_closureSized = Debug.unDCS -> Debug.ArrWordsClosure{bytes, arrWords}}} -> do
      let payload = arrWordsBS (take (fromIntegral bytes) arrWords)
      Scotty.setHeader "Content-Type" "application/octet-stream"
      Scotty.setHeader "Content-Disposition" "attachment; filename=\"closure.bin\""
      Scotty.raw payload
arrDumpProf (ClosureLine cs) = dumpArrWord cs 
arrDumpProf _ = mempty
arrDumpCount (FieldLine cs) = dumpArrWord cs 
arrDumpCount _ = mempty
arrDumpThunk _ = mempty

handleConnect appStateRef state formValue options isValid connect = do
  let match = F.find (\s -> TL.fromStrict (socketName s) == formValue) options
  case match of
    Just socketLike -> do
      valid <- liftIO $ isValid socketLike
      if valid
        then do
          debuggee <- liftIO $ connect (writeBChan (_appChan state) . ProgressMessage)
                                 (_socketLocation socketLike)
          let newState = state & majorState .~ Connected
                      { _debuggeeSocket = socketLike
                      , _debuggee = debuggee
                      , _mode = RunningMode
                      }
          liftIO $ writeIORef appStateRef newState
          Scotty.redirect "/connect"
        else Scotty.html renderBadSocketPage
    Nothing -> Scotty.html renderBadSocketPage

handleImg :: IOTree a name -> (IOTreeNode a name -> String) -> String -> (a -> String) -> [Int] -> Scotty.ActionM ()
handleImg tree nodeName pageName format' selectedPath = do
  let subtree = getSubTree tree selectedPath
  (expSubtree, capped) <- liftIO $ expandNodeSafe subtree format'
  let name = nodeName subtree
  let (nodes', vizEdges) = getClosureVizTree format' Set.empty [] expSubtree
  let vizNodes = Set.toList nodes'
  liftIO $ do
    createDirectoryIfMissing True "tmp"
    let graph = buildClosureGraph vizNodes vizEdges
    _ <- runGraphviz graph Svg svgPath
    return ()
  Scotty.html $ renderImgPage pageName name selectedPath capped

  

app :: IORef AppState -> Scotty.ScottyM ()
app appStateRef = do
  {- Serves the visualisation of the selected object -}
  Scotty.get "/graph" $ do
    Scotty.setHeader "Content-Type" "image/svg+xml"
    Scotty.file svgPath
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
  {- Main page where sockets/snapshots can be selected for debugging -}
  Scotty.get "/" $ do
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Setup st knownDebuggees knownSnapshots -> do
        knownDebuggees' <- liftIO $ updateListFrom socketDirectory knownDebuggees
        knownSnapshots' <- liftIO $ updateListFrom snapshotDirectory knownSnapshots
        let socketList = F.toList $ knownDebuggees' ^. listElementsL
        let snapshotList = F.toList $ knownSnapshots' ^. listElementsL
        liftIO $ writeIORef appStateRef $ state & majorState .~ Setup st knownDebuggees' knownSnapshots'
        case st of 
          Socket -> Scotty.html $ renderSocketSelectionPage st socketList
          Snapshot -> Scotty.html $ renderSocketSelectionPage st snapshotList
      Connected {} -> do
        Scotty.html $ renderAlreadyConnectedPage
  {- GET version of /connect, in case / is accessed while already connected to a debuggee -}
  Scotty.get "/connect" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected socket debuggee mode -> 
        case mode of 
          PausedMode os -> do
            let tree = _treeSavedAndGCRoots os
            let getName x = case x of ClosureDetails{} -> Just (closureFormat x); _ -> Nothing
            let getSize' x = case x of ClosureDetails _ excSize _ -> getSize excSize
            inc <- liftIO $ getIncSize getName getSize' tree selectedPath
            Scotty.html $ renderConnectedPage selectedPath (Just inc) socket debuggee mode
          _ -> Scotty.html $ renderConnectedPage selectedPath Nothing socket debuggee mode
      _ -> Scotty.redirect "/"
  {- Here debuggees can be paused and resumed. When paused, information about closures can be displayed -}
  Scotty.post "/connect" $ do
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Setup st knownDebuggees knownSnapshots -> do
        socketParam <- Scotty.formParam "socket"
        case st of
          Snapshot -> do
            let snapshots = F.toList (knownSnapshots ^. listElementsL)
            handleConnect appStateRef state socketParam snapshots (\ _ -> pure True) snapshotConnect 
          Socket -> do
            let sockets = F.toList (knownDebuggees ^. listElementsL)
            handleConnect appStateRef state socketParam sockets
                          (\s -> isSocketAlive (_socketLocation s)) debuggeeConnect
      Connected socket debuggee mode -> do
        selectedPath <- selectedParam Scotty.queryParam        
        case mode of
          PausedMode os -> do
            let tree = _treeSavedAndGCRoots os
            let getName x = case x of ClosureDetails{} -> Just (closureFormat x); _ -> Nothing
            let getSize' x = case x of ClosureDetails _ excSize _ -> getSize excSize
            inc <- liftIO $ getIncSize getName getSize' tree selectedPath
            Scotty.html $ renderConnectedPage selectedPath (Just inc) socket debuggee mode
          _ -> Scotty.html $ renderConnectedPage selectedPath Nothing socket debuggee mode
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
      Connected socket debuggee RunningMode -> do
        liftIO $ pause debuggee
        ver <- liftIO $ GD.version debuggee
        (rootsTree, initRoots) <- liftIO $ mkSavedAndGCRootsIOTree debuggee
        let pausedState = PausedMode $
                            OperationalState Nothing
                              Nothing
                              savedAndGCRoots
                              NoOverlay
                              FooterInfo
                              (DefaultRoots initRoots)
                              rootsTree
                              (_appChan state)
                              (Just 100)
                              []
                              ver 
            newAppState = state & majorState . mode .~ pausedState
        liftIO $ writeIORef appStateRef newAppState
        Scotty.redirect "/connect"     
      _ -> Scotty.redirect "/"
  {- Resumes debuggee -}
  Scotty.post "/resume" $ do
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Connected socket debuggee (PausedMode _) -> do
        liftIO $ resume debuggee
        let newAppState = state & majorState . mode .~ RunningMode
        liftIO $ writeIORef appStateRef newAppState
        Scotty.redirect "/connect"
      _ -> Scotty.redirect "/"  
  {- Kill process or exit to selection screen -}
  Scotty.post "/exit" $ do
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Connected socket debuggee (PausedMode os) -> do
        case view running_task os of
          Just tid -> do
            liftIO $ killThread tid
            let newAppState = state & majorState . mode . pausedMode . running_task .~ Nothing
                                    & majorState . mode . pausedMode %~ resetFooter
            liftIO $ writeIORef appStateRef newAppState
            Scotty.redirect "/connect" 
          Nothing -> do
            liftIO $ resume debuggee
            let newAppState = initialAppState (_appChan state)
            liftIO $ writeIORef appStateRef newAppState
            Scotty.redirect "/"
      _ -> Scotty.redirect "/"
  {- Toggles the expansion state of a path in the tree -}
  Scotty.post "/toggle" $ do
    toggleIx <- togglePathParam Scotty.formParam
    selectedPath <- selectedParam Scotty.formParam
    let newSelected = toggleSelected selectedPath toggleIx
    let selectedStr = encodePath newSelected
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Connected socket debuggee (PausedMode os) -> do
        case _treeMode os of
          SavedAndGCRoots _ -> do 
            let tree = _treeSavedAndGCRoots os
            newTree <- liftIO $ toggleTreeByPath tree toggleIx
            let newAppState = state & majorState . mode . pausedMode . treeSavedAndGCRoots .~ newTree
            liftIO $ writeIORef appStateRef newAppState
            Scotty.redirect $ "/connect?selected=" <> TL.fromStrict selectedStr
          Retainer f tree -> do
            newTree <- liftIO $ toggleTreeByPath tree toggleIx
            let newOs = os { _treeMode = Retainer f newTree }
                newMajorState = Connected socket debuggee (PausedMode newOs)
                newAppState = state & majorState .~ newMajorState
            liftIO $ writeIORef appStateRef newAppState
            Scotty.redirect $ "/searchWithFilters?selected=" <> TL.fromStrict selectedStr
          SearchedHtml f tree name -> do
            newTree <- liftIO $ toggleTreeByPath tree toggleIx
            let newOs = os { _treeMode = SearchedHtml f newTree name }
                newMajorState = Connected socket debuggee (PausedMode newOs)
                newAppState = state & majorState .~ newMajorState
            liftIO $ writeIORef appStateRef newAppState
            Scotty.redirect $ "/" <> TL.pack name <> "?selected=" <> TL.fromStrict selectedStr
      _ -> Scotty.redirect "/"
  {- Creates and displays the graph for the selected object -}
  Scotty.post "/img" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.formParam
    case state ^. majorState of
      Connected socket debuggee mode ->
        case mode of
          PausedMode os -> do
            case _treeMode os of
              SavedAndGCRoots _ -> do
                let tree = _treeSavedAndGCRoots os
                handleImg tree getNodeName "connect" closureFormat selectedPath
              Retainer _ tree -> do
                handleImg tree getNodeName "searchWithFilters" closureFormat selectedPath
              SearchedHtml Utils{..} tree pageName -> do
                let nameFn = maybe "" id . _getName 
                handleImg tree (\(IOTreeNode n' _) -> nameFn n') pageName nameFn selectedPath
  {- View profile (level 1 and 2) -}
  genericGet appStateRef "/profile" renderProfilePage
  Scotty.post "/profile" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected socket debuggee mode ->
        case mode of
          PausedMode os -> do
            level <- profileLevelParam Scotty.formParam
            profMap <- liftIO $ profile debuggee level "profile_dump" 
            let sortedProfiles = Prelude.reverse $
                  [ ProfileLine k kargs v
                  | ((k, kargs), v) <- List.sortBy (comparing (cssize . snd)) (M.toList profMap)
                  ]
                totalStats = foldMap snd (M.toList profMap)
            
                gChildren _ (ClosureLine c) = map ClosureLine <$> getChildren debuggee c
                gChildren _ (ProfileLine _ _ stats) = do
                  let samples = getSamples (sample stats)
                  closures <- forM samples $ \ptr -> do
                    deref <- run debuggee $ GD.dereferenceClosure ptr
                    return $ ListFullClosure $ Closure ptr deref
                  filled <- forM (zip [0 :: Int ..] closures) $ \(i, c) -> do
                    filledC <- fillListItem debuggee c
                    return (show i, filledC)
                  mapM (\(lbl, filledItem) -> ClosureLine <$> getClosureDetails debuggee (pack lbl) filledItem) filled
            let tree = mkIOTree debuggee sortedProfiles gChildren renderProfileLine id
            let getName x = case x of ClosureLine c -> Just (closureFormat c); _ -> Nothing
            let getSize' x = case x of
                               ClosureLine (ClosureDetails _ excSize _) -> getSize excSize
                               _ -> 0
            inc <- liftIO $ getIncSize getName getSize' tree selectedPath
            let newOs = os { _treeMode = SearchedHtml (Utils renderProfileHtml (renderProfileSummary totalStats) arrDumpProf getName getSize') tree "profile" }
                newMajorState = Connected socket debuggee (PausedMode newOs)
                newAppState = state & majorState .~ newMajorState
            liftIO $ writeIORef appStateRef newAppState
            Scotty.html $ renderProfilePage selectedPath (Just inc) (_mode newMajorState)
  {- Returns to /connect, sets the treeMode -}
  Scotty.post "/reconnect" $ do
    state <- liftIO $ readIORef appStateRef
    case state ^. majorState of
      Connected socket debuggee mode ->
        case mode of
          PausedMode os -> do
            let newOs = os { _treeMode = SavedAndGCRoots undefined }
                newMajorState = Connected socket debuggee (PausedMode newOs)
                newAppState = state & majorState .~ newMajorState
            liftIO $ writeIORef appStateRef newAppState
            Scotty.redirect "/connect"
  {- Downloads the arr_words payload -}
  Scotty.get "/dumpArrWords" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected socket debuggee mode ->
        case mode of
          PausedMode os -> do
            case _treeMode os of
              SavedAndGCRoots _ -> do
                let (IOTreeNode node _) = getSubTree (_treeSavedAndGCRoots os) selectedPath
                dumpArrWord node 
              Retainer _ tree -> do
                let (IOTreeNode node _) = getSubTree tree selectedPath
                dumpArrWord node
              SearchedHtml Utils{..} tree _  -> do
                let (IOTreeNode node _) = getSubTree tree selectedPath
                _dumpArrWords node
  {- See arr_words count -}
  genericGet appStateRef "/arrWordsCount" (renderCountPage "ARR_WORDS")
  Scotty.post "/arrWordsCount" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected socket debuggee mode ->
        case mode of
          PausedMode os -> do
            arrMap <- liftIO $ arrWordsAnalysis Nothing debuggee
            let all_res = Prelude.reverse $ 
                  [ (k, S.toList v ) 
                  | (k, v) <- (List.sortBy (comparing 
                                           (\(k, v) -> fromIntegral (BS.length k) * S.size v)) 
                                           (M.toList arrMap))
                  ]
                display_res = maybe id take (_resultSize os) all_res
                top_closure = [CountLine k (fromIntegral (BS.length k)) (length v) | (k, v) <- display_res]

                !words_histogram = Just $ histogramHtml 8 (concatMap (\(k, bs) -> let sz = BS.length k in replicate (length bs) (Size (fromIntegral sz))) all_res)
                
                g_children d (CountLine b _ _) = do
                  let Just cs = M.lookup b arrMap
                  cs' <- run debuggee $ forM (S.toList cs) $ \c -> do
                    c' <- GD.dereferenceClosure c
                    return $ ListFullClosure $ Closure c c'
                  children' <- traverse (traverse (fillListItem d)) $ zipWith (\n c -> (show @Int n, c)) [0..] cs'
                  mapM (\(lbl, child) -> FieldLine <$> getClosureDetails d (pack lbl) child) children'
                g_children d (FieldLine c) = map FieldLine <$> getChildren d c

            let tree = mkIOTree debuggee top_closure g_children renderArrWordsLines id
            let getName x = case x of FieldLine c -> Just (closureFormat c); _ -> Nothing
            let getSize' x = case x of
                               FieldLine (ClosureDetails _ excSize _) -> getSize excSize
                               _ -> 0
   
            inc <- liftIO $ getIncSize getName getSize' tree selectedPath

            let newOs = os { _treeMode = SearchedHtml (Utils renderCountHtml (renderCountSummary words_histogram) arrDumpCount getName getSize') tree "arrWordsCount" }
                newMajorState = Connected socket debuggee (PausedMode newOs)
                newAppState = state & majorState .~ newMajorState
            liftIO $ writeIORef appStateRef newAppState
            Scotty.html $ renderCountPage "ARR_WORDS" selectedPath (Just inc) (_mode newMajorState)
  {- See strings count -}
  genericGet appStateRef "/stringsCount" (renderCountPage "Strings")
  Scotty.post "/stringsCount" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected socket debuggee mode ->
        case mode of
          PausedMode os -> do
            stringMap <- liftIO $ stringsAnalysis Nothing debuggee
            let sorted_res = maybe id take (_resultSize os) $ 
                             Prelude.reverse [(k, S.toList v ) 
                                             | (k, v) <- (List.sortBy (comparing (S.size . snd)) 
                                             (M.toList stringMap))]
            
                top_closure = [CountLine k (length k) (length v) | (k, v) <- sorted_res]
               
                g_children d (CountLine b _ _) = do
                  let Just cs = M.lookup b stringMap
                  cs' <- run debuggee $ forM (S.toList cs) $ \c -> do
                    c' <- GD.dereferenceClosure c
                    return $ ListFullClosure $ Closure c c'
                  children' <- traverse (traverse (fillListItem d)) $ zipWith (\n c -> (show @Int n, c)) [0..] cs'
                  mapM (\(lbl, child) -> FieldLine <$> getClosureDetails d (pack lbl) child) children'
                g_children d (FieldLine c) = map FieldLine <$> getChildren d c

            let tree = mkIOTree debuggee top_closure g_children renderArrWordsLines id
            let getName x = case x of FieldLine c -> Just (closureFormat c); _ -> Nothing
            let getSize' x = case x of
                               FieldLine (ClosureDetails _ excSize _) -> getSize excSize
                               _ -> 0
   
            inc <- liftIO $ getIncSize getName getSize' tree selectedPath

            let newOs = os { _treeMode = SearchedHtml (Utils renderCountHtml (renderCountSummary Nothing) arrDumpCount getName getSize') tree "stringsCount" }
                newMajorState = Connected socket debuggee (PausedMode newOs)
                newAppState = state & majorState .~ newMajorState
            liftIO $ writeIORef appStateRef newAppState
            Scotty.html $ renderCountPage "Strings" selectedPath (Just inc) (_mode newMajorState)
  {- Thunk analysis -}
  genericGet appStateRef "/thunkAnalysis" renderThunkAnalysisPage
  Scotty.post "/thunkAnalysis" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected socket debuggee mode ->
        case mode of
          PausedMode os -> do
            thunkMap <- liftIO $ thunkAnalysis debuggee
            let top_closure = Prelude.reverse [ ThunkLine k v | (k, v) <- (List.sortBy (comparing (getCount . snd)) (M.toList thunkMap))]

                g_children _ (ThunkLine {}) = pure []
            let tree = mkIOTree debuggee top_closure g_children undefined id
            let newOs = os { _treeMode = SearchedHtml (Utils renderThunkAnalysisHtml renderThunkAnalysisSummary arrDumpThunk (const Nothing) (const 0)) tree "thunkAnalysis" }
                newMajorState = Connected socket debuggee (PausedMode newOs)
                newAppState = state & majorState .~ newMajorState
            liftIO $ writeIORef appStateRef newAppState
            Scotty.html $ renderThunkAnalysisPage selectedPath Nothing (_mode newMajorState)
  {- Take snapshot -}
  Scotty.post "/takeSnapshot" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.formParam
    filename <- Scotty.formParam "filename"
    case state ^. majorState of
      Connected socket debuggee mode -> do
        liftIO $ snapshot debuggee filename
        Scotty.redirect ("/connect?selected=" <> TL.fromStrict (encodePath selectedPath))
  {- Search with filters -}
  Scotty.get "/searchWithFilters" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected socket debuggee mode -> do
        case mode of
          PausedMode os -> do
            case _treeMode os of
              Retainer _ tree -> do
                let getName x = case x of ClosureDetails{} -> Just (closureFormat x); _ -> Nothing
                let getSize' x = case x of ClosureDetails _ excSize _ -> getSize excSize
                inc <- liftIO $ getIncSize getName getSize' tree selectedPath
                Scotty.html $ renderFilterSearchPage selectedPath (Just inc) mode
  Scotty.post "/searchWithFilters" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected socket debuggee mode ->
        case mode of
          PausedMode os -> do
            let mClosFilter = uiFiltersToFilter (_filters os)
            cps <- liftIO $ retainersOf (_resultSize os) mClosFilter Nothing debuggee
            let cps' = map (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
            res <- liftIO $ mapM (mapM (completeClosureDetails debuggee)) cps'
            let tree = mkRetainerTree debuggee res
            let newOs = os { _treeMode = Retainer renderClosureDetails tree }
                newMajorState = Connected socket debuggee (PausedMode newOs)
                newAppState = state & majorState .~ newMajorState
            let getName x = case x of ClosureDetails{} -> Just (closureFormat x); _ -> Nothing
            let getSize' x = case x of ClosureDetails _ excSize _ -> getSize excSize
            inc <- liftIO $ getIncSize getName getSize' tree selectedPath
            liftIO $ writeIORef appStateRef newAppState
            Scotty.html $ renderFilterSearchPage selectedPath (Just inc) (_mode newMajorState)
  Scotty.get "/modifyFilters" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected socket debuggee mode ->
        Scotty.html $ renderModifyFilterPage mode
  Scotty.post "/modifyFilters" $ do
    state <- liftIO $ readIORef appStateRef
    selectedPath <- selectedParam Scotty.queryParam
    case state ^. majorState of
      Connected socket debuggee mode ->
        Scotty.html $ renderModifyFilterPage mode
  Scotty.post "/addFilter" $ do
    state <- liftIO $ readIORef appStateRef
    pattern :: String <- patternParam Scotty.formParam
    filterType :: String <- filterTypeParam Scotty.formParam
    invert <- invertParam Scotty.formParam
    case state ^. majorState of 
      Connected socket debuggee mode -> 
        case mode of
          PausedMode os -> do
            let newFilter = case filterType of 
                              "Address" -> maybe [] (pure . UIAddressFilter invert) (readClosurePtr pattern)
                              "InfoAddress" -> maybe [] (pure . UIInfoAddressFilter invert) (readInfoTablePtr pattern) 
                              "ConstrName" -> [UIConstructorFilter invert pattern]
                              "ClosureName" -> [UIInfoNameFilter invert pattern]
                              "ClosureSize" -> maybe [] (pure . UISizeFilter invert) (readMaybe pattern) 
                              "ClosureType" -> maybe [] (pure . UIClosureTypeFilter invert) (readMaybe pattern) 
            let newOs = addFilters newFilter os  
                newMajorState = Connected socket debuggee (PausedMode newOs)
                newAppState = state & majorState .~ newMajorState
            liftIO $ writeIORef appStateRef newAppState
            Scotty.redirect "/modifyFilters"
  Scotty.post "/deleteFilter" $ do
    state <- liftIO $ readIORef appStateRef
    index <- indexParam Scotty.formParam
    case state ^. majorState of 
      Connected socket debuggee mode -> 
        case mode of
          PausedMode os -> do
            let newFilters = let (as, bs) = Prelude.splitAt index (_filters os) in as ++ (tail bs)
                newOs = setFilters newFilters os
                newMajorState = Connected socket debuggee (PausedMode newOs)
                newAppState = state & majorState .~ newMajorState
            liftIO $ writeIORef appStateRef newAppState
            Scotty.redirect "/modifyFilters"

     
  where mkSavedAndGCRootsIOTree debuggee' = do
          raw_roots <- take 1000 . map ("GC Roots",) <$> GD.rootClosures debuggee'
          rootClosures' <- liftIO $ mapM (completeClosureDetails debuggee') raw_roots
          raw_saved <- map ("Saved Object",) <$> GD.savedClosures debuggee'
          savedClosures' <- liftIO $ mapM (completeClosureDetails debuggee') raw_saved
          return $ (mkIOTree debuggee' (savedClosures' ++ rootClosures') getChildren renderInlineClosureDesc id
                   , fmap toPtr <$> (raw_roots ++ raw_saved))


main :: IO ()
main = newMain

oldMain :: IO ()
oldMain = do
  eventChan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan eventChan PollTick
    -- 2s
    threadDelay 2_000_000
  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  
  let app' :: App AppState Event Name
      app' = App
        { appDraw = myAppDraw
        , appChooseCursor = showFirstCursor
        , appHandleEvent = myAppHandleEvent
        , appStartEvent = myAppStartEvent
        , appAttrMap = myAppAttrMap
        }
  _finalState <- customMain initialVty buildVty
                    (Just eventChan) app' (initialAppState eventChan)
  return ()

newMain :: IO ()
newMain = do
  eventChan <- newBChan 10
  let initial = initialAppState eventChan
  appStateRef <- newIORef initial
  Scotty.scotty 3000 (app appStateRef)
