{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}

module IOTree
  ( IOTree
  , IOTreePath
  , RowState(..)
  , RowCtx(..)
  , ioTree
  , setIOTreeRoots
  , getIOTreeRoots
  , renderIOTree
  , handleIOTreeEvent
  , ioTreeSelection
  , ioTreeToggle

  , ioTreeViewSelection
  , unViewTree
  , viewPath
  , viewSelect
  , viewUp
  , viewUp'
  , viewUnsafeDown
  , viewPrevSibling
  , viewNextSibling
  , viewCollapse
  , viewExpand
  , viewIsCollapsed

  , renderIOTreeHtml
  , renderIOSummary
  ) where

import Lucid

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

renderIOTree :: (Show name, Ord name) => IOTree node name -> Widget name
renderIOTree iotree
  = drawTreeElements iotree

data TreeNodeWithRenderContext node = TreeNodeWithRenderContext
  { _nodeDepth :: Int
  , _nodeState ::  RowState
  , _nodeSelected :: Bool
  , _nodeLast :: RowCtx
  , _nodeParentLast :: [RowCtx]
  , _nodeContent :: node
  }

renderTreeNodeWithContext ::
  (RowState -> Bool -> RowCtx -> [RowCtx] -> node -> Widget name) ->
  TreeNodeWithRenderContext node -> Widget name
renderTreeNodeWithContext rowRenderer ctx =
  rowRenderer (_nodeState ctx) (_nodeSelected ctx) (_nodeLast ctx) (_nodeParentLast ctx) (_nodeContent ctx)

drawTreeElements :: (Ord n, Show n) => IOTree node n -> Widget n
drawTreeElements (IOTree widgetName treeNodes _ renderRow pathTop) =
  -- This function takes inspiration from 'Brick.Widget.List.drawListElements'
  Widget Greedy Greedy $ do
    c <- getContext

    -- Take (numPerHeight * 2) elements, or whatever is left
    let
      rs = flattenTree 0 [] treeNodes pathTop
      es = take (numPerHeight * 2) $ drop start rs

      idx = fromMaybe 0 (List.findIndex (_nodeSelected) rs)

      start = max 0 $ idx - numPerHeight + 1

      -- We hardcode the height of each element to be expected as 1 row.
      -- Perhaps we could greedily compute the number of elements based on
      -- their dynamic height for a perfect result,
      -- but it currently feels like overkill.
      itemHeight = 1

      -- The number of items to show is the available height
      -- divided by the item height...
      initialNumPerHeight = (c^.availHeightL) `div` itemHeight
      -- ... but if the available height leaves a remainder of
      -- an item height then we need to ensure that we render an
      -- extra item to show a partial item at the top or bottom to
      -- give the expected result when an item is more than one
      -- row high. (Example: 5 rows available with item height
      -- of 3 yields two items: one fully rendered, the other
      -- rendered with only its top 2 or bottom 2 rows visible,
      -- depending on how the viewport state changes.)
      numPerHeight = initialNumPerHeight +
                      if initialNumPerHeight * itemHeight == c^.availHeightL
                      then 0
                      else 1

      off = start * itemHeight

    render $ viewport widgetName Vertical $
              translateBy (Location (0, off)) $
              vBox $ (map (renderTreeNodeWithContext renderRow) es)
  where
    -- Compute metadata for each row we may display.
    -- This is a logical representation of each row that can
    -- be split and filtered later.
    -- Each 'TreeNodeWithRenderContext' can be individually rendered without
    -- further issue.
    flattenTree _ _ [] _ = []
    flattenTree minorIx depth (IOTreeNode node' csE : ns) path = case csE of
      -- Collapsed
      Left _ -> row Collapsed : rowsRest
      -- Expanded
      Right cs -> row (Expanded (null cs))
                    : flattenTree 0 (rowCtx : depth) cs (if childIsSelected then drop 1 path else [])
                      ++ rowsRest
      where
      childIsSelected = case path of
        x:_ -> x == minorIx
        _ -> False
      selected = path == [minorIx]
      rowCtx = if null ns then LastRow else NotLastRow
      row state = TreeNodeWithRenderContext
        { _nodeDepth = length depth
        , _nodeState = state
        , _nodeSelected = selected
        , _nodeLast = rowCtx
        , _nodeParentLast = depth
        , _nodeContent = node'
        }
      rowsRest = flattenTree (minorIx + 1) depth ns path

handleIOTreeEvent :: Vty.Event -> IOTree node name -> EventM name s (IOTree node name)
handleIOTreeEvent e tree
  = liftIO
  $ forIOTreeViewSelection tree
  $ \view -> fmap viewSelect $ case e of
    Vty.EvKey KRight _ -> do
        (view', cs) <- viewExpand view
        return $ if null cs then view' else viewUnsafeDown view' 0
    Vty.EvKey KDown _ -> return $ next view
    Vty.EvKey KLeft [Vty.MShift] -> return $ viewCollapseAll view
    Vty.EvKey KLeft _ -> return $ viewCollapse $ fromMaybe view (viewUp' view)
    Vty.EvKey KUp _ -> return $ prev view
    Vty.EvKey KPageDown _ -> return $ List.foldl' (flip ($)) view (replicate 15 next)
    Vty.EvKey KPageUp _ -> return $ List.foldl' (flip ($)) view (replicate 15 prev)
    _ -> return view
    where
      next v = fromMaybe v (viewNextVisible v)
      prev v = fromMaybe v (viewPrevVisible v)

-- | Toggle (expanded/collapsed) at the current selection.
ioTreeToggle :: IOTree node name -> IO (IOTree node name)
ioTreeToggle t = forIOTreeViewSelection t $ \view ->
  if viewIsCollapsed view
  then fst <$> viewExpand view
  else return (viewCollapse view)

-- | A view (or Zipper) used to navigate the tree
data IOTreeView node name
  = Root (IOTree node name)
  | Node
      (IOTreeNode node name -> IOTreeView node name) -- reconstruct the parent given this node
      Int -- The index in the parent
      (IOTreeNode node name) -- This node

forIOTreeViewSelection
  :: IOTree node name
  -> (IOTreeView node name -> IO (IOTreeView node name))
  -> IO (IOTree node name)
forIOTreeViewSelection t f = unViewTree <$> f (ioTreeViewSelection t)

ioTreeViewSelection :: IOTree node name -> IOTreeView node name
ioTreeViewSelection t = List.foldl' viewUnsafeDown (Root t) (_selection t)

ioTreeSelection :: IOTree node name -> Maybe node
ioTreeSelection t = case ioTreeViewSelection t of
  Root{} -> Nothing
  Node _ _ n -> Just (_node n)

unViewTree :: IOTreeView node name -> IOTree node name
unViewTree t = case t of
    Root t' -> t'
    Node mkParent _ t' -> unViewTree (mkParent t')

-- | Current path in the tree
viewPath :: IOTreeView node name -> [Int]
viewPath tTop = reverse $ go tTop
  where
  go t = case t of
    Root _ -> []
    Node mkParent i t' -> i : go (mkParent t')

-- | Select the current path
viewSelect :: IOTreeView node name -> IOTreeView node name
viewSelect t = ioTreeViewSelection newTree
  where
  newTree = oldTree { _selection = newSelection }
  oldTree = unViewTree t
  newSelection = viewPath t

-- | move up the tree
viewUp :: IOTreeView node name -> Maybe (IOTreeView node name)
viewUp t = case t of
  Root{} -> Nothing
  Node mkParent _ t' -> Just (mkParent t')

-- | move up the tree, but never to the root
viewUp' :: IOTreeView node name -> Maybe (IOTreeView node name)
viewUp' t = case viewUp t of
  Just Root{} -> Nothing
  x -> x

-- | Move down to a child in the tree. Index must be in range. Must be expanded.
viewUnsafeDown :: HasCallStack => IOTreeView node name -> Int -> IOTreeView node name
viewUnsafeDown view i
  | viewIsCollapsed view = error "viewUnsafeDown: view must be expanded"
  | otherwise = case view of
      Root t -> Node (\c -> Root t{ _roots = listSet i c (_roots t) }) i (t !. i)
      Node mkParent ixInParent t -> Node
                  (\c -> Node mkParent ixInParent (unsafeSetChild c i t))
                  i
                  (t ! i)

viewPrevVisible :: HasCallStack => IOTreeView node name -> Maybe (IOTreeView node name)
viewPrevVisible view = case viewPrevSibling view of
  Nothing -> viewUp' view
  Just nextSib -> Just (viewLastVisibleChild nextSib)
  where
  viewLastVisibleChild view' = if viewIsCollapsed view'
    then view'
    else let
      n = case view' of
            Root t -> length (_roots t) - 1
            Node _ _ t -> either (error "Impossible! view' is expanded") length (_children t)
      in if n == 0 then view' else viewLastVisibleChild $ viewUnsafeDown view' (n-1)

viewNextVisible :: HasCallStack => IOTreeView node name -> Maybe (IOTreeView node name)
viewNextVisible view = let
  upwardNext v = case viewNextSibling v of
    Nothing -> upwardNext =<< viewUp v
    Just s -> Just s
  in viewFirstVisibleChild view <|> upwardNext view
  where
  viewFirstVisibleChild view' = if viewIsCollapsed view'
    then Nothing
    else let
      nullChildren = case view' of
            Root t -> null (_roots t)
            Node _ _ t -> either (error "Impossible! view' is expanded") null (_children t)
      in if nullChildren then Nothing else Just (viewUnsafeDown view' 0)

-- | Move to the previous sibling within the parent node
viewPrevSibling :: HasCallStack => IOTreeView node name -> Maybe (IOTreeView node name)
viewPrevSibling t = case t of
  Root{} -> Nothing
  Node mkParent ixInParent t' -> if ixInParent == 0
    then Nothing
    else Just $ viewUnsafeDown (mkParent t') (ixInParent - 1)

-- | Move to the next sibling within the parent node
viewNextSibling :: HasCallStack => IOTreeView node name -> Maybe (IOTreeView node name)
viewNextSibling t = case t of
  Root{} -> Nothing
  Node mkParent ixInParent t' -> let
    parent = mkParent t'
    nSiblings = case parent of
        Root t'' -> length (_roots t'')
        Node _ _ t'' -> length (either (error "Impossible! syblings must be expanded") id (_children t''))
    in if ixInParent + 1 == nSiblings
        then Nothing
        else Just (viewUnsafeDown parent (ixInParent + 1))

-- | Collapse the current node.
viewCollapse :: HasCallStack => IOTreeView node name -> IOTreeView node name
viewCollapse t = case t of
  Root _ -> t -- Can't collapse the root
  Node mkParent i t' -> case _children t' of
    Left _ -> t
    Right cs -> Node mkParent i t'{_children = Left (return cs)}

-- | Collapse the current node and all the nodes in the subtree rooted at
-- the current node.
viewCollapseAll :: HasCallStack => IOTreeView node name -> IOTreeView node name
viewCollapseAll tv = case tv of
    Root t            -> Root (t {_roots = fmap go (_roots t)})
    Node mkParent i t -> case _children t of
      Left cs  -> Node mkParent i t {_children = Left $ fmap go <$> cs}
      Right cs -> Node mkParent i t {_children = Left . pure $ fmap go cs }
  where
    go :: IOTreeNode node name -> IOTreeNode node name
    go tn = case _children tn of
      Left cs  -> tn {_children = Left $ fmap go <$> cs }
      Right cs -> tn {_children = Left . pure $ fmap go cs}

-- | Expand the current node. Returns the children of the current node.
viewExpand :: HasCallStack => IOTreeView node name -> IO (IOTreeView node name, [IOTreeNode node name])
viewExpand t = case t of
  Root t' -> return (t, _roots t')
  Node mkParent i t' -> case _children t' of
    Left getChildren -> do
      cs <- getChildren
      return (Node mkParent i t'{_children=Right cs}, cs)
    Right cs -> return (t, cs)


viewIsCollapsed :: HasCallStack => IOTreeView node name -> Bool
viewIsCollapsed t = case t of
  Root{} -> False
  Node _ _ t' -> case _children t' of
    Left{} -> True
    Right{} -> False

(!.) :: IOTree node name -> Int -> IOTreeNode node name
t !. i = _roots t !! i

(!) :: IOTreeNode node name -> Int -> IOTreeNode node name
t ! i = case _children t of
  Right xs -> xs !! i
  Left _ -> error "(!): tree node not expanded"

unsafeSetChild ::  HasCallStack => IOTreeNode node name -> Int -> IOTreeNode node name -> IOTreeNode node name
unsafeSetChild newChild i t = case _children t of
  Right xs -> t { _children = Right (listSet i newChild xs) }
  Left _ -> error "(!): tree node not expanded"

listSet :: HasCallStack => Int -> a -> [a] -> [a]
listSet i a as
  | i >= length as = error $ "listSet: index (" ++ show i ++ ") out of bounds [0 - " ++ show (length as) ++ ")"
  | otherwise = take i as ++ [a] ++ drop (i+1) as


flattenTreeHtml :: [RowCtx] -> Int -> [IOTreeNode node name] -> [Int] -> [TreeNodeWithRenderContext node]
flattenTreeHtml _ _ [] _ = []
flattenTreeHtml depth minorIx (IOTreeNode node' csE : ns) path =
  currentRow ++ children ++ flattenTreeHtml depth (minorIx + 1) ns path
  where selected = path == [minorIx]
        childIsSelected = case path of
          (x:_) -> x == minorIx
          _  -> False
        rowCtx = if null ns then LastRow else NotLastRow
        row state = TreeNodeWithRenderContext {
          _nodeDepth = length depth,
          _nodeState = state,
          _nodeSelected = selected,
          _nodeLast = rowCtx,
          _nodeParentLast = depth,
          _nodeContent = node'
        }
        (currentRow, children) = case csE of
          Left _ -> ([row Collapsed], [])
          Right cs -> let r = row (Expanded (null cs))
                          cs' = flattenTreeHtml (rowCtx : depth) 0 cs (if childIsSelected then drop 1 path else [])
                      in ([r], cs')

renderTreeRowHtml :: (RowState -> Bool -> RowCtx -> [RowCtx] -> node -> Html ())
                  -> TreeNodeWithRenderContext node
                  -> Html ()
renderTreeRowHtml rowRenderer TreeNodeWithRenderContext{..} =
  rowRenderer _nodeState _nodeSelected _nodeLast _nodeParentLast _nodeContent

renderTreeRowHtmlWithIndex :: Int
                           -> (Int -> RowState -> Bool -> RowCtx -> [RowCtx] -> node -> Html ())
                           -> TreeNodeWithRenderContext node
                           -> Html ()
renderTreeRowHtmlWithIndex idx rowRenderer TreeNodeWithRenderContext{..} =
  rowRenderer idx _nodeState _nodeSelected _nodeLast _nodeParentLast _nodeContent

renderIOTreeHtml :: (Ord name, Show name) => IOTree node name 
                                          -> (Int -> RowState -> Bool -> RowCtx -> [RowCtx] -> node -> Html ())
                                          -> Html ()
renderIOTreeHtml (IOTree _ roots _ _ selection) renderRow =
  let tree = flattenTreeHtml [] 0 roots selection
  in div_ [class_ "iotree"] $
       mconcat $ --map (renderTreeRowHtml renderRow) tree
         zipWith (\idx node -> 
                    renderTreeRowHtmlWithIndex idx renderRow node)
                  [0..] tree  

renderIOSummary
  :: (Ord name, Show name)
  => IOTree node name
  -> Int
  -> (RowState -> RowCtx -> [RowCtx] -> node -> Html ())
  -> Html ()
renderIOSummary (IOTree _ roots _ _ selection) i renderSummary = 
  let tree = flattenTreeHtml [] 0 roots selection
  in div_ [class_ "iotree-container"] $ do
       case safeIndex tree i of
         Just (TreeNodeWithRenderContext{..}) -> 
           renderSummary _nodeState _nodeLast _nodeParentLast _nodeContent
         Nothing -> mempty
  where safeIndex :: [a] -> Int -> Maybe a
        safeIndex xs n
          | n < 0 || n >= length xs = Nothing
          | otherwise = Just (xs!!n)
        
