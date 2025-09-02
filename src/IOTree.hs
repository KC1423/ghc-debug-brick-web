{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}

module IOTree
  ( IOTree(..)
  , IOTreePath
  , RowState(..)
  , RowCtx(..)
  , ioTree
  , setIOTreeRoots
  , getIOTreeRoots

  , renderIOTreeHtml
  , IOTreeNode(..)
  , getSubTree
  , toggleTreeByPath
  , expandNodeWithCap
  , expandNodeSafe
  ) where

import Lucid
import qualified Data.Set as Set

import           Brick
import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Maybe (fromMaybe)
import qualified Data.List as List
import           GHC.Stack
import qualified Graphics.Vty.Input.Events as Vty
import           Graphics.Vty.Input.Events (Key(..))
import           Lens.Micro ((^.))


-- A tree style list where items can be expanded and collapsed
data IOTree node name = IOTree
    { _name :: name
    , _roots :: [IOTreeNode node name]
    , _getChildren :: (node -> IO [node])
    , _renderRow :: RowState     -- is row expanded?
                 -> Bool         -- is row selected?
                 -> RowCtx       -- is current node last in subtree?
                 -> [RowCtx]     -- per level of tree depth, are parent nodes last in subtree?
                 -> node         -- the node to render
                 -> Widget name
    -- Render some extra info as the first child of each node
    , _selection :: [Int]
    -- ^ Indices along the path to the current selection. Empty list means no
    -- selection.
    }

data RowState = Expanded Bool | Collapsed
data RowCtx = NotLastRow | LastRow

setIOTreeRoots :: [node] -> IOTree node name ->  IOTree node name
setIOTreeRoots newRoots iot = iot { _roots = (nodeToTreeNode (_getChildren iot) <$> newRoots) }

getIOTreeRoots :: IOTree node name -> [node]
getIOTreeRoots iot = map _node (_roots iot)



type IOTreePath node = [(Int, node)]

data IOTreeNode node name
  = IOTreeNode
    { _node :: node
      -- ^ Current node
    , _children :: Either
        (IO [IOTreeNode node name])  -- Node is collapsed
        [IOTreeNode node name]       -- Node is expanded
    }

ioTree
  :: forall node name
  .  name
  -- ^ Name of the tree
  -> [node]
  -- ^ Root nodes
  -> (node -> IO [node])
  -- ^ Get child nodes of a node
  -> (RowState        -- is row expanded or collapsed?
      -> Bool         -- Is row selected
      -> RowCtx       -- innermost context
      -> [RowCtx]     -- Tree depth
      -> node         -- the node to render
      -> Widget name)
  -- ^ Row renderer (should add it's own indent based on depth)
  -> IOTree node name
ioTree name rootNodes getChildrenIO renderRow
  = IOTree
    { _name = name
    , _roots = nodeToTreeNode getChildrenIO <$> rootNodes
    , _getChildren = getChildrenIO
    , _renderRow = renderRow
    , _selection = if null rootNodes then [] else [0]
    -- ^ TODO we could take the initial path but we'd have to expand through to
    -- that path with IO
    }
  where

nodeToTreeNode :: (node -> IO [node]) -> node -> IOTreeNode node name
nodeToTreeNode k n = IOTreeNode n (Left (fmap (nodeToTreeNode k) <$> k n))

data TreeNodeWithRenderContext node = TreeNodeWithRenderContext
  { _nodeDepth :: Int
  , _nodeState ::  RowState
  , _nodeSelected :: Bool
  , _nodeLast :: RowCtx
  , _nodeParentLast :: [RowCtx]
  , _nodeContent :: node
  }

{-
-- | A view (or Zipper) used to navigate the tree
data IOTreeView node name
  = Root (IOTree node name)
  | Node
      (IOTreeNode node name -> IOTreeView node name) -- reconstruct the parent given this node
      Int -- The index in the parent
      (IOTreeNode node name) -- This node
-}

{- New code / web stuff -}

flattenTreeHtml
  :: [RowCtx]
  -> Int
  -> [IOTreeNode node name]
  -> [Int]
  -> [Int]
  -> [RenderTree (TreeNodeWithRenderContext node)]
flattenTreeHtml _ _ [] _ _ = []
flattenTreeHtml depth minorIx (IOTreeNode node' csE : ns) selection parentPath =
  case csE of
    Left _ -> RenderNode (row Collapsed) [] : rest
    Right cs -> RenderNode (row (Expanded True)) 
                  (flattenTreeHtml (rowCtx : depth) 0 cs 
                      (if childIsSelected then drop 1 selection else []) thisPath)
                  : rest
  where rest = flattenTreeHtml depth (minorIx + 1) ns selection parentPath
        thisPath = parentPath ++ [minorIx]
        childIsSelected = selection `isChildOf` thisPath
        rowCtx = if null ns then LastRow else NotLastRow
        row state = TreeNodeWithRenderContext {
          _nodeDepth = length depth,
          _nodeState = state,
          _nodeSelected = False,
          _nodeLast = rowCtx,
          _nodeParentLast = depth,
          _nodeContent = node'
        }
        isChildOf :: Eq a => [a] -> [a] -> Bool
        isChildOf (x:xs) (y:ys) = x == y && isChildOf xs ys
        isChildOf [] _ = True
        isChildOf _ [] = False


renderIOTreeHtml :: (Ord name, Show name) => IOTree node name 
                                          -> [Int]
                                          -> ([Int] -> [Int] -> Bool -> Bool -> node -> Html ())
                                          -> Html ()
renderIOTreeHtml (IOTree _ roots _ _ _) selectedPath renderRow =
  let tree = flattenTreeHtml [] 0 roots [] []
  in div_ [class_ "iotree"] $
       go [] tree
  where go _ [] = mempty
        go parentPath trees = mconcat $ zipWith renderOne [0..] trees
          where renderOne ix (RenderNode TreeNodeWithRenderContext{..} children) =
                  let thisPath = parentPath ++ [ix]
                      selected = thisPath == selectedPath
                      expanded = case _nodeState of 
                                   Expanded True -> True
                                   _ -> False
                      rowHtml = renderRow selectedPath thisPath expanded selected _nodeContent
                      childHtml = go thisPath children
                  in rowHtml <> childHtml

getSubTree :: IOTree node name -> [Int] -> Maybe (IOTreeNode node name)
getSubTree (IOTree _ roots _ _ _) path = findNodeByPath roots path
  where findNodeByPath :: [IOTreeNode node name] -> [Int] -> Maybe (IOTreeNode node name)
        findNodeByPath _ [] = Nothing
        findNodeByPath [] _ = Nothing
        findNodeByPath (n@(IOTreeNode _ csE) : rest) (i:is) = 
          if i == 0 then if null is then Just n else case csE of
                                                       Left _ -> Nothing
                                                       Right cs -> findNodeByPath cs is
           else findNodeByPath rest (i-1 : is)


toggleTreeByPath :: IOTree node name -> [Int] -> IO (IOTree node name)
toggleTreeByPath (IOTree a roots b c d) path = do
  newRoots <- toggleNodeByPath roots path
  return $ IOTree a newRoots b c d

toggleNodeByPath :: [IOTreeNode node name] -> [Int] -> IO [IOTreeNode node name]
toggleNodeByPath [] _ = return []
toggleNodeByPath _ [] = return []
toggleNodeByPath (n@(IOTreeNode node' csE) : rest) (i:is) =
  if i == 0 then if null is then case csE of
                                   Left getChildren -> do
                                     cs <- getChildren
                                     return $ IOTreeNode node' (Right cs) : rest                
                                   Right cs -> return $ IOTreeNode node' (Left $ return cs) : rest
                            else case csE of
                                   Left getChildren -> do
                                     csE' <- getChildren
                                     ecsE' <- toggleNodeByPath csE' is
                                     return $ IOTreeNode node' (Right ecsE') : rest
                                   Right cs -> do
                                     ecs <- toggleNodeByPath cs is
                                     return $ IOTreeNode node' (Right ecs) : rest
            else do
               rest' <- toggleNodeByPath rest (i-1 : is)
               return $ n : rest'
 
expandNodeSafe :: IOTreeNode node name -> (node -> String) -> IO (IOTreeNode node name, Bool)
expandNodeSafe = expandNodeWithCap 100

expandNodeWithCap :: Int -> IOTreeNode node name -> (node -> String) -> IO (IOTreeNode node name, Bool)
expandNodeWithCap cap n' format = do
  (node, nodes) <- go Set.empty n'
  return (node, Set.size nodes == cap)
  where 
    go seen n@(IOTreeNode node csE)
      | cap == Set.size seen = return (n, seen)
      | otherwise = let ptr = format node
                    in if Set.member ptr seen
                       then return (n, seen) 
                       else case csE of
                         Left getChildren -> do
                           cs <- getChildren
                           (newCs', seen') <- processChildren (Set.insert ptr seen) cs
                           return (IOTreeNode node (Right newCs'), seen')
                         Right cs -> do
                           (newCs', seen') <- processChildren (Set.insert ptr seen) cs
                           return (IOTreeNode node (Right newCs'), seen')
    processChildren seen [] = return ([], seen)
    processChildren seen (c:cs)
      | cap == Set.size seen = return ([], seen)
      | otherwise = do
          (c', seen') <- go seen c
          (cs', seen'') <- processChildren seen' cs
          return (c':cs', seen'')

data RenderTree a = RenderNode a [RenderTree a]
