{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model
  ( module Model
  , module Namespace
  , module Common
  ) where

import Data.Maybe (fromMaybe)
import Data.Sequence as Seq
import Lens.Micro.Platform
import Data.Time
import System.Directory
import System.FilePath
import Data.Text(Text, pack)
import qualified Data.Text as T
import Text.Read

import Brick.Forms
import Brick.BChan
import Brick (EventM, Widget)
import Brick.Widgets.List

import Namespace
import Common
import Lib
import IOTree
import Control.Concurrent
import qualified Graphics.Vty as Vty
import Data.Int
import GHC.Debug.Client (ccID)
import GHC.Debug.Client.Monad (DebugM)
import GHC.Debug.CostCentres (findAllChildrenOfCC)
import qualified GHC.Debug.Types as GD
import qualified GHC.Debug.Types.Version as GD


data Event
  = PollTick  -- Used to perform arbitrary polling based tasks e.g. looking for new debuggees
  | ProgressMessage Text
  | ProgressFinished Text NominalDiffTime
  | AsyncFinished (EventM Name OperationalState ())


initialAppState :: BChan Event -> AppState
initialAppState event = AppState
  { _majorState = Setup
      { _setupKind = Socket
      , _knownDebuggees = list Setup_KnownDebuggeesList [] 1
      , _knownSnapshots = list Setup_KnownSnapshotsList [] 1
      },
    _appChan = event
  }

data AppState = AppState
  { _majorState :: MajorState
  , _appChan    :: BChan Event
  }

mkSocketInfo :: FilePath -> IO SocketInfo
mkSocketInfo fp = SocketInfo fp <$> getModificationTime fp

socketName :: SocketInfo -> Text
socketName = pack . takeFileName . _socketLocation

renderSocketTime :: SocketInfo -> Text
renderSocketTime = pack . formatTime defaultTimeLocale "%c" . _socketCreated

data SocketInfo = SocketInfo
                    { _socketLocation :: FilePath -- ^ FilePath to socket, absolute path
                    , _socketCreated :: UTCTime  -- ^ Time of socket creation
                    } deriving Eq

instance Ord SocketInfo where
  compare (SocketInfo s1 t1) (SocketInfo s2 t2) =
    -- Compare time first
    compare t1 t2 <> compare s1 s2

type SnapshotInfo = SocketInfo

data SetupKind = Socket | Snapshot

toggleSetup :: SetupKind -> SetupKind
toggleSetup Socket = Snapshot
toggleSetup Snapshot = Socket

data MajorState
  -- | We have not yet connected to a debuggee.
  = Setup
    { _setupKind :: SetupKind
    , _knownDebuggees :: GenericList Name Seq SocketInfo
    , _knownSnapshots :: GenericList Name Seq SnapshotInfo
    }

  -- | Connected to a debuggee
  | Connected
    { _debuggeeSocket :: SocketInfo
    , _debuggee :: Debuggee
    , _mode     :: ConnectedMode
    }

data InfoInfo = InfoInfo
  {
    _labelInParent :: Text -- ^ A label describing the relationship to the parent
  -- Stuff  that requires IO to calculate
  , _pretty :: Text
  , _sourceLocation :: Maybe SourceInformation
  , _closureType :: Maybe Text
  , _constructor :: Maybe Text
  , _profHeaderInfo :: !(Maybe ProfHeaderWord)
  } deriving Show

data ClosureDetails = ClosureDetails
  { _closure :: DebugClosure CCSPtr SrtCont PayloadCont ConstrDesc StackCont ClosurePtr
  , _excSize :: Size
  , _info :: InfoInfo
  }
  | InfoDetails { _info :: InfoInfo }
  | CCSDetails Text CCSPtr (GenCCSPayload CCSPtr CCPayload)
  | CCDetails Text CCPayload
  | LabelNode { _label :: Text } deriving Show

data TreeMode = SavedAndGCRoots (ClosureDetails -> Widget Name)
              | Retainer (ClosureDetails -> Widget Name) (IOTree (ClosureDetails) Name)
              | forall a . Searched (a -> Widget Name) (IOTree a Name)

treeLength :: TreeMode -> Maybe Int
treeLength (SavedAndGCRoots {}) = Nothing
treeLength (Retainer _ tree) = Just $ Prelude.length $ getIOTreeRoots tree
treeLength (Searched _ tree) = Just $ Prelude.length $ getIOTreeRoots tree

data FooterMode = FooterInfo
                | FooterMessage Text
                | FooterInput FooterInputMode (Form Text () Name)

isFocusedFooter :: FooterMode -> Bool
isFocusedFooter (FooterInput {}) = True
isFocusedFooter _ = False

data FooterInputMode = FClosureAddress {runNow :: Bool, invert :: Bool}
                     | FInfoTableAddress {runNow :: Bool, invert :: Bool}
                     | FConstructorName {runNow :: Bool, invert :: Bool}
                     | FClosureName {runNow :: Bool, invert :: Bool}
                     | FArrWordsSize
                     | FFilterEras {runNow :: Bool, invert :: Bool}
                     | FFilterClosureType {invert :: Bool}
                     | FFilterClosureSize {invert :: Bool}
                     | FFilterCcId {runNow :: Bool, invert :: Bool}
                     | FProfile ProfileLevel
                     | FSnapshot
                     | FDumpArrWords
                     | FSetResultSize
                     deriving Show

-- | Profiling requirement for a command
data ProfilingReq
  = ReqSomeProfiling
  | ReqErasProfiling
  | NoReq

data Command = Command { commandDescription :: Text
                       , commandKey :: Maybe Vty.Event
                       , dispatchCommand :: Debuggee -> EventM Name OperationalState ()
                       , commandRequiresProfMode :: ProfilingReq
                       -- ^ The command requires that the debuggee is in a specific
                       -- profiling mode.
                       -- For example, we can only filter by eras if the program
                       -- has era profiling enabled.
                       }

mkCommand :: Text -> Vty.Event -> EventM Name OperationalState () -> Command
mkCommand desc ev dispatch = Command desc (Just ev) (\_ -> dispatch) NoReq

mkCommand' :: Text -> EventM Name OperationalState () -> Command
mkCommand' desc dispatch = Command desc Nothing (\_ -> dispatch) NoReq

mkFilterCmd :: Text -> Vty.Event -> EventM Name OperationalState () -> ProfilingReq -> Command
mkFilterCmd desc ev dispatch profMode = Command desc (Just ev) (\_ -> dispatch) profMode

mkFilterCmd' :: Text -> EventM Name OperationalState () -> ProfilingReq -> Command
mkFilterCmd' desc dispatch profMode = Command desc Nothing (\_ -> dispatch) profMode

isCmdEnabled :: GD.Version -> Command -> Bool
isCmdEnabled debuggeeVersion cmd = case commandRequiresProfMode cmd of
  NoReq -> True
  ReqErasProfiling ->
    Just GD.EraProfiling == GD.v_profiling debuggeeVersion
  ReqSomeProfiling ->
    GD.isProfiledRTS debuggeeVersion

isCmdDisabled :: GD.Version -> Command -> Bool
isCmdDisabled v cmd = not $ isCmdEnabled v cmd

data OverlayMode = KeybindingsShown
                 -- TODO: Abstract the "CommandPicker" into it's own module
                 | CommandPicker (Form Text () Name) (GenericList Name Seq Command) (Seq Command)
                 | NoOverlay


invertInput :: FooterInputMode -> FooterInputMode
invertInput x@FClosureAddress{invert} = x{invert = not invert}
invertInput x@FInfoTableAddress{invert} = x{invert = not invert}
invertInput x@FConstructorName{invert} = x{invert = not invert}
invertInput x@FClosureName{invert} = x{invert = not invert}
invertInput x@FFilterEras{invert} = x{invert = not invert}
invertInput x@FFilterClosureSize{invert} = x{invert = not invert}
invertInput x@FFilterClosureType{invert} = x{invert = not invert}
invertInput x@FFilterCcId{invert} = x{invert = not invert}
invertInput x = x

formatFooterMode :: FooterInputMode -> Text
formatFooterMode FClosureAddress{invert} = (if invert then "!" else "") <> "address (0x..): "
formatFooterMode FInfoTableAddress{invert} = (if invert then "!" else "") <> "info table pointer (0x..): "
formatFooterMode FConstructorName{invert} = (if invert then "!" else "") <> "constructor name: "
formatFooterMode FClosureName{invert} = (if invert then "!" else "") <> "closure name: "
formatFooterMode FFilterEras{invert} = (if invert then "!" else "") <> "era range (<era>/<start-era>-<end-era>): "
formatFooterMode FFilterClosureSize{invert} = (if invert then "!" else "") <> "closure size (bytes): "
formatFooterMode FFilterClosureType{invert} = (if invert then "!" else "") <> "closure type: "
formatFooterMode FFilterCcId{invert} = (if invert then "!" else "") <> "CC Id: "
formatFooterMode FArrWordsSize = "size (bytes)>= "
formatFooterMode FDumpArrWords = "dump payload to file: "
formatFooterMode FSetResultSize = "search result limit (0 for infinity): "
formatFooterMode FSnapshot = "snapshot name: "
formatFooterMode (FProfile {}) = "filename: "

data ConnectedMode
  -- | Debuggee is running
  = RunningMode
  -- | Debuggee is paused and we're exploring the heap
  | PausedMode { _pausedMode :: OperationalState }

data RootsOrigin = DefaultRoots [(Text, Ptr)]
                 | SearchedRoots [(Text, Ptr)]


currentRoots :: RootsOrigin -> [(Text, Ptr)]
currentRoots (DefaultRoots cp) = cp
currentRoots (SearchedRoots cp) = cp

data OperationalState = OperationalState
    { _running_task :: Maybe ThreadId
    , _last_run_time :: Maybe (Text, NominalDiffTime)
    , _treeMode :: TreeMode
    , _keybindingsMode :: OverlayMode
    , _footerMode :: FooterMode
    , _rootsFrom  :: RootsOrigin
    , _treeSavedAndGCRoots :: IOTree (ClosureDetails) Name
    -- ^ Tree corresponding to SavedAndGCRoots mode
    , _event_chan :: BChan Event
    , _resultSize :: Maybe Int
    , _filters :: [UIFilter]
    , _version :: GD.Version
    }

clearFilters :: OperationalState -> OperationalState
clearFilters os = os { _filters = [] }

setFilters :: [UIFilter] -> OperationalState -> OperationalState
setFilters fs os = os {_filters = fs}

addFilters :: [UIFilter] -> OperationalState -> OperationalState
addFilters fs os = os {_filters = fs ++ _filters os}

data UIFilter =
    UIAddressFilter Bool ClosurePtr
  | UIInfoAddressFilter Bool InfoTablePtr
  | UIConstructorFilter Bool String
  | UIInfoNameFilter Bool String
  | UIEraFilter Bool EraRange
  | UISizeFilter Bool Size
  | UIClosureTypeFilter Bool ClosureType
  | UICcId Bool Int64

uiFiltersToFilter :: [UIFilter] -> DebugM ClosureFilter
uiFiltersToFilter uifilters = do
  closFilters <- mapM uiFilterToFilter uifilters
  pure $ foldr AndFilter (PureFilter True) closFilters

uiFilterToFilter :: UIFilter -> DebugM ClosureFilter
uiFilterToFilter (UIAddressFilter invert x)     = pure $ AddressFilter (xor invert . (== x))
uiFilterToFilter (UIInfoAddressFilter invert x) = pure $ InfoPtrFilter (xor invert . (== x))
uiFilterToFilter (UIConstructorFilter invert x) = pure $ ConstructorDescFilter (xor invert . (== x) . name)
uiFilterToFilter (UIInfoNameFilter invert x)    = pure $ InfoSourceFilter (xor invert . (== x) . infoName)
uiFilterToFilter (UIEraFilter  invert x)        = pure $ ProfHeaderFilter (xor invert . (`profHeaderInEraRange` (Just x)))
uiFilterToFilter (UISizeFilter invert x)        = pure $ SizeFilter (xor invert . (>= x))
uiFilterToFilter (UIClosureTypeFilter invert x) = pure $ InfoFilter (xor invert . (== x) . tipe)
uiFilterToFilter (UICcId invert x)      = do
  ccsPtrs <- findAllChildrenOfCC ((x ==) . ccID)
  pure $ ProfHeaderFilter (xor invert . (`profHeaderReferencesCCS` ccsPtrs))

xor :: Bool -> Bool -> Bool
xor False False = False
xor True True = False
xor _ _ = True

parseEraRange :: Text -> Maybe EraRange
parseEraRange range = case T.splitOn "-" range of
  [nstr] -> case readMaybe (T.unpack nstr) of
    Just n -> Just $ EraRange n n
    Nothing -> Nothing
  [start,end] -> case (T.unpack start, T.unpack end) of
    ("", "") -> Just $ EraRange 0 maxBound
    ("", readMaybe -> Just e) -> Just $ EraRange 0 e
    (readMaybe -> Just s, "") -> Just $ EraRange s maxBound
    (readMaybe -> Just s, readMaybe -> Just e) -> Just $ EraRange s e
    _ -> Nothing
  _ -> Nothing

showEraRange :: EraRange -> String
showEraRange (EraRange s e)
  | s == e = show s
  | otherwise = "[" ++ show s ++ "," ++ go e
  where
    go n
      | n == maxBound = "∞)"
      | otherwise = show n ++ "]"

osSize :: OperationalState -> Int
osSize os = fromMaybe (Prelude.length (getIOTreeRoots $ _treeSavedAndGCRoots os)) $ treeLength (_treeMode os)

pauseModeTree :: (forall a . (a -> Widget Name) -> IOTree a Name -> r) -> OperationalState -> r
pauseModeTree k (OperationalState _ _ mode _kb _footer _from roots _ _ _ _) = case mode of
  SavedAndGCRoots render -> k render roots
  Retainer render r -> k render r
  Searched render r -> k render r

makeLenses ''AppState
makeLenses ''MajorState
makeLenses ''ClosureDetails
makeLenses ''ConnectedMode
makeLenses ''OperationalState
makeLenses ''SocketInfo
makeLenses ''OverlayMode
