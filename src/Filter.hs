module Filter where

import GHC.Debug.Client.Query (dereferenceConDesc, dereferenceCCS, dereferenceCC, getSourceInfo)
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class
import qualified Data.List as List
import qualified GHC.Debug.Types.Closures as Debug
import Lib as GD
import GHC.Debug.Client.Monad

data Suggestions = Suggestions 
  { _cons :: [String]     -- Constructors
  , _cloNames :: [String] -- Exact constructors
  , _cloTypes :: [String] -- Closure types
  , _ccIds :: [String]    -- Cost centre ids
  }

getSuggestions :: MonadIO m => GHC.Debug.Client.Monad.DebugM ClosureFilter 
                            -> Debuggee -> m Suggestions
getSuggestions mClosFilter debuggee' = do 
  forSearch <- liftIO $ retainersOf Nothing mClosFilter Nothing debuggee'
  constrs <- getConstructors debuggee' forSearch
  cloNames <- getClosureNames debuggee' forSearch
  cloTypes <- getClosureTypes forSearch
  ccIds <- getCcIds debuggee' forSearch
  return $ Suggestions constrs cloNames cloTypes ccIds


{- Individual filters -}
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
