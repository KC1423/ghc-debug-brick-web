{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}

module IOTree
  ( IOTree(..)
  , ioTree

  , renderIOTreeHtml
  , IOTreeNode(..)
  , getSubTree
  , toggleTreeByPath
  , expandNodeWithCap
  , expandNodeSafe
  ) where

import Lucid
import qualified Data.Set as Set

-- A tree style list where items can be expanded and collapsed
data IOTree node name = IOTree
    { _name :: name
    , _roots :: [IOTreeNode node name]
    , _selection :: [Int]
    -- ^ Indices along the path to the current selection. Empty list means no
    -- selection.
    }
 
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
  -> IOTree node name
ioTree name rootNodes getChildrenIO
  = IOTree
    { _name = name
    , _roots = nodeToTreeNode getChildrenIO <$> rootNodes
    , _selection = if null rootNodes then [] else [0]
    -- ^ TODO we could take the initial path but we'd have to expand through to
    -- that path with IO
    }

nodeToTreeNode :: (node -> IO [node]) -> node -> IOTreeNode node name
nodeToTreeNode k n = IOTreeNode n (Left (fmap (nodeToTreeNode k) <$> k n))


{- New code / web stuff -}

renderIOTreeHtml :: (Ord name, Show name) => IOTree node name 
                                          -> [Int]
                                          -> ([Int] -> [Int] -> Bool -> Bool -> node -> Html ())
                                          -> Html ()
renderIOTreeHtml (IOTree _ roots _) selectedPath renderRow =
  div_ [class_ "iotree"] $
    go [] roots
  where go _ [] = mempty
        go parentPath trees = mconcat $ zipWith renderOne [0..] trees
          where renderOne ix (IOTreeNode nodeContent children) =
                  let thisPath = parentPath ++ [ix]
                      selected = thisPath == selectedPath
                      expanded = case children of
                                   Left _ -> False
                                   Right _ -> True
                      rowHtml = renderRow selectedPath thisPath expanded selected nodeContent
                      childHtml = go thisPath (either (const []) id children)
                  in rowHtml <> childHtml

getSubTree :: IOTree node name -> [Int] -> Maybe (IOTreeNode node name)
getSubTree (IOTree _ roots _) path = findNodeByPath roots path
  where findNodeByPath :: [IOTreeNode node name] -> [Int] -> Maybe (IOTreeNode node name)
        findNodeByPath _ [] = Nothing
        findNodeByPath [] _ = Nothing
        findNodeByPath (n@(IOTreeNode _ csE) : rest) (i:is) = 
          if i == 0 then if null is then Just n else case csE of
                                                       Left _ -> Nothing
                                                       Right cs -> findNodeByPath cs is
           else findNodeByPath rest (i-1 : is)


toggleTreeByPath :: IOTree node name -> [Int] -> IO (IOTree node name)
toggleTreeByPath (IOTree a roots b) path = do
  newRoots <- toggleNodeByPath roots path
  return $ IOTree a newRoots b

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
