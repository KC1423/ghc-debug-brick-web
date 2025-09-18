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
  , renderTreeNodesHtml
  , IOTreeNode(..)
  , getSubTree
  , toggleTreeByPath
  , expandNodeWithCap
  , expandNodeSafe
  , NodeInfo(..)
  ) where

import Lucid
import qualified Data.Set as Set
import qualified Data.Text as T

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

data NodeInfo = NodeInfo
  { formatted :: String
  , path :: [Int]
  , expanded :: Bool
  , children :: [String]
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
getChildrenE :: Either (IO [IOTreeNode node name]) [IOTreeNode node name] -> IO [IOTreeNode node name]
getChildrenE csE = case csE of
                     Left getChildren' -> getChildren'
                     Right cs -> pure cs


renderIOTreeHtml :: (Ord name, Show name) => IOTree node name 
                                          -> [Int]
                                          -> ([Int] -> Bool -> Bool -> node -> Html ())
                                          -> ([Int] -> T.Text)
                                          -> Html ()
renderIOTreeHtml (IOTree _ roots _) selectedPath renderRow encode =
  div_ [id_ "iotree"] $
    renderTreeNodesHtml renderRow selectedPath [] roots encode
  
renderTreeNodesHtml :: ([Int] -> Bool -> Bool -> a -> Html ())
                    -> [Int] -> [Int] -> [IOTreeNode a name] -> ([Int] -> T.Text) -> Html ()
renderTreeNodesHtml renderRow selectedPath parentPath nodes encode = 
  mconcat $ zipWith renderOne [0..] nodes
  where
    renderOne ix (IOTreeNode content children) = 
      let thisPath = parentPath ++ [ix]
          selected = thisPath == selectedPath
          expanded = case children of
                       Right _ -> True
                       Left _ -> False
          rowHtml = renderRow thisPath expanded selected content
          childHtml =
            case children of
              Right cs -> 
                div_
                  [ id_ ("children-" <> encode thisPath)
                  , class_ "children"
                  , data_ "loaded" "true"
                  , style_ "display: block;"
                  ] $
                    renderTreeNodesHtml renderRow selectedPath thisPath cs encode
              Left _ -> mempty
      in rowHtml <> childHtml               

getSubTree :: IOTree node name -> [Int] -> Maybe (IOTreeNode node name)
getSubTree (IOTree _ roots _) path = findNodeByPath roots path
  where findNodeByPath :: [IOTreeNode node name] -> [Int] -> Maybe (IOTreeNode node name)
        findNodeByPath _ [] = Nothing
        findNodeByPath [] _ = Nothing
        findNodeByPath (n : _) [0] = Just n
        findNodeByPath (IOTreeNode _ csE : _) (0 : is) = case csE of
          Left _ -> Nothing
          Right cs -> findNodeByPath cs is
        findNodeByPath (_ : rest) (i : is) = findNodeByPath rest (i-1 : is)


toggleTreeByPath :: IOTree node name -> [Int] -> IO (IOTree node name)
toggleTreeByPath (IOTree a roots b) path = do
  newRoots <- toggleNodeByPath roots path
  return $ IOTree a newRoots b

toggleNodeByPath :: [IOTreeNode node name] -> [Int] -> IO [IOTreeNode node name]
toggleNodeByPath [] _ = return []
toggleNodeByPath _ [] = return []
toggleNodeByPath (IOTreeNode node' csE : rest) [0] =
  case csE of
    Left getChildren -> do
      cs <- getChildren
      return $ IOTreeNode node' (Right cs) : rest                
    Right cs -> return $ IOTreeNode node' (Left $ return cs) : rest
toggleNodeByPath (IOTreeNode node' csE : rest) (0 : is) = do
  cs <- getChildrenE csE
  ecs <- toggleNodeByPath cs is
  return $ IOTreeNode node' (Right ecs) : rest
toggleNodeByPath (n : rest) (i : is) = do
  rest' <- toggleNodeByPath rest (i-1 : is)
  return $ n : rest'
 
expandNodeSafe :: IOTreeNode node name -> (node -> String) -> [[Int]] -> IO (IOTreeNode (node, [Int], Bool) name, Bool)
expandNodeSafe = expandNodeWithCap 100 []

expandNodeWithCap :: Int -> [Int] -> IOTreeNode node name -> (node -> String) -> [[Int]] -> IO (IOTreeNode (node, [Int], Bool) name, Bool)
expandNodeWithCap cap root n' format forced = do
  (node, nodes) <- go Set.empty n' root 0
  return (node, Set.size nodes >= cap)
  where 
    go seen (IOTreeNode node csE) pathSoFar minorIx = do
      let ptr = format node
          thisPath = pathSoFar ++ [minorIx]
      if Set.member ptr seen then return (IOTreeNode (node, thisPath, False) (Right []), seen)
      else do
        let seen'' = Set.insert ptr seen
        cs <- getChildrenE csE
        if canExpand thisPath seen'' cs then do
          (newCs', seen', expanded) <- processChildren seen'' thisPath 0 cs
          return (IOTreeNode (node, thisPath, expanded) (Right newCs'), seen')
        else return (IOTreeNode (node, thisPath, False) (wrapChildren csE), seen'')
    processChildren seen _ _ [] = return ([], seen, True)
    processChildren seen thisPath minorIx (c:cs)
      | canExpand thisPath seen (c:cs) = do
          (c', seen') <- go seen c thisPath minorIx
          (cs', seen'', expanded) <- processChildren seen' thisPath (minorIx + 1) cs
          return (c':cs', seen'', expanded)
      | otherwise = return ([], seen, False)
    allChildrenSeen seen' cs = and (map (\(IOTreeNode x _) -> Set.member (format x) seen') cs)
    canExpand path seen' cs = Set.size seen' < cap || path `elem` forced || allChildrenSeen seen' cs
    wrapChildren csE = Left $ do
      cs <- getChildrenE csE
      return $ fmap (\c -> IOTreeNode (getNode c, [], False) (Left (return []))) cs
    getNode (IOTreeNode n _) = n
