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

import Network.Socket as NS
import Control.Exception (try, SomeException)
import Web.Scotty.Internal.Types
import Lucid
import qualified Data.Text.Lazy as TL
import Data.GraphViz
import Control.Concurrent.Async

import Lens.Micro.Platform
import Data.Time
import System.Directory
import System.FilePath
import Data.Text(Text, pack)
import qualified Data.Text as T
import Text.Read

import Namespace
import Common
import Lib
import IOTree
import Data.Int
import GHC.Debug.Client (ccID)
import GHC.Debug.Client.Monad (DebugM)
import GHC.Debug.CostCentres (findAllChildrenOfCC)
import qualified GHC.Debug.Types as GD
import qualified GHC.Debug.Types.Version as GD

initialAppState :: AppState
initialAppState = AppState
  { _majorState = Setup
      { _setupKind = Socket
      , _knownDebuggees = []
      , _knownSnapshots = []
      }
  , currentTask = Nothing
  }

data AppState = AppState
  { _majorState :: MajorState
  , currentTask :: Maybe (Async ())
  }

data Suggestions = Suggestions 
  { _cons :: [String]
  , _cloNames :: [String]
  , _cloTypes :: [String]
  , _ccIds :: [String]
  }

mkSocketInfo :: FilePath -> IO SocketInfo
mkSocketInfo fp = SocketInfo fp <$> getModificationTime fp

socketName :: SocketInfo -> Text
socketName = pack . takeFileName . _socketLocation

renderSocketTime :: SocketInfo -> Text
renderSocketTime = pack . formatTime defaultTimeLocale "%c" . _socketCreated

isSocketAlive :: FilePath -> IO Bool
isSocketAlive path = do
  result <- try $ do
    socket' <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
    NS.connect socket' (NS.SockAddrUnix path)
    NS.close socket'
  pure $ case result of
    Left (_ :: SomeException) -> False
    Right _ -> True

data SocketInfo = SocketInfo
                    { _socketLocation :: FilePath -- ^ FilePath to socket, absolute path
                    , _socketCreated :: UTCTime  -- ^ Time of socket creation
                    } deriving Eq

instance Ord SocketInfo where
  compare (SocketInfo s1 t1) (SocketInfo s2 t2) =
    -- Compare time first
    compare t1 t2 <> compare s1 s2

type SnapshotInfo = SocketInfo

data SetupKind = Socket | Snapshot deriving (Eq, Show)

toggleSetup :: SetupKind -> SetupKind
toggleSetup Socket = Snapshot
toggleSetup Snapshot = Socket

data MajorState
  -- | We have not yet connected to a debuggee.
  = Setup
    { _setupKind :: SetupKind
    , _knownDebuggees :: [SocketInfo] --GenericList Name Seq SocketInfo
    , _knownSnapshots :: [SnapshotInfo] --GenericList Name Seq SnapshotInfo
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

data ImgInfo = ImgInfo
  { _name :: String 
  , _capped :: Bool
  , _svgContent :: GraphvizCommand -> IO ()
  }

data CDIO = CDIO 
  { _mInc :: Maybe (Int, Bool)
  , _imgInfo :: Maybe ImgInfo
  , _hasGV :: Bool
  }

data Utils a = Utils {
  _renderRow :: a -> Html (),
  _renderSummary :: a -> [Int] -> CDIO -> Html (),
  _graphFormat :: a -> String,
  _dumpArrWords :: a -> Web.Scotty.Internal.Types.ActionT IO (),
  _getName :: a -> Maybe String,
  _getSize :: a -> Int
}

data TreeMode = SavedAndGCRoots
              | Retainer (IOTree (ClosureDetails) Name) Suggestions
              | forall a . SearchedHtml (Utils a) (IOTree a Name) String

-- | Profiling requirement for a command
data ProfilingReq
  = ReqSomeProfiling
  | ReqErasProfiling
  | NoReq

inEraMode :: GD.Version -> Bool
inEraMode ver = Just GD.EraProfiling == GD.v_profiling ver
inSomeProfMode :: GD.Version -> Bool
inSomeProfMode ver = GD.isProfiledRTS ver

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
    { _treeMode :: TreeMode
    , _rootsFrom  :: RootsOrigin
    , _treeSavedAndGCRoots :: IOTree (ClosureDetails) Name
    -- ^ Tree corresponding to SavedAndGCRoots mode
    , _resultSize :: Maybe Int
    , _filters :: [UIFilter]
    , _version :: GD.Version
    , _genSvg :: GraphvizCommand -> IO ()
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

makeLenses ''AppState
makeLenses ''MajorState
makeLenses ''ClosureDetails
makeLenses ''ConnectedMode
makeLenses ''OperationalState
makeLenses ''SocketInfo
