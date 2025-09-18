{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Graph where

import qualified Data.Text.Lazy as TL
import qualified Data.Set as Set
import Data.GraphViz (GraphvizCommand(..), style, filled, color, X11Color(Yellow, Red, Green, GreenYellow), fillColor) 
import Data.GraphViz.Attributes.Complete (Attribute(Overlap, URL, Shape, Label, TailPort), Overlap(PrismOverlap), Shape(Record), Label(StrLabel), PortName(..), PortPos(..))
import Data.GraphViz.Types
import Data.GraphViz.Types.Monadic (node, edge, digraph)
import Data.GraphViz.Types.Generalised (DotGraph, graphStatements, DotStatement(GA))
import qualified Data.List as List
import qualified Data.Text as T
import System.Process
import System.IO
import IOTree
import Model

type EdgeList = [(String, String, Int)]

getClosureVizTree :: (a -> String) -> (a -> String) -> Set.Set String -> [(String, NodeInfo)] -> EdgeList -> IOTreeNode (a, [Int], Bool) name -> (Set.Set String, [(String, NodeInfo)], EdgeList)
getClosureVizTree getName' format' nodes formattedNodes edges (IOTreeNode (n, path', expanded') csE) = 
  let ptr = getName' n
  in if Set.member ptr nodes
     then (nodes, formattedNodes, [])
     else case csE of
            Left _ -> (Set.insert ptr nodes, (ptr, (NodeInfo (format' n) path' False [])) : formattedNodes, [])
            Right cs -> 
              let nodes'' = Set.insert ptr nodes
                  (nodesFinal, fNodesFinal, childEdges) = listApply (getClosureVizTree getName' format') (nodes'', fnodes'', edges) cs
                  children' = [ getName' n'
                              | IOTreeNode (n', _, _) _ <- cs ]
                  newEdges = map (\(ch, i) -> (ptr, ch, i)) (zip children' [0..])
                  fnodes'' = (ptr, NodeInfo (format' n) path' expanded' children') : formattedNodes
              in (nodesFinal, fNodesFinal, childEdges ++ newEdges)
  where
    listApply f (ns, fns, es) xs =
      foldl (\(nsAcc, fnsAcc, esAcc) x ->
               let (ns', fns', es') = f nsAcc fnsAcc [] x
               in (ns', fns', esAcc ++ es')) (ns, fns, es) xs

buildClosureGraph :: [String] -> [(String, NodeInfo)] -> EdgeList -> Data.GraphViz.Types.Generalised.DotGraph String
buildClosureGraph nodes fnodes edges = digraph (Str "Visualisation") $ do
  mapM_ (\n -> 
            let NodeInfo fNode path' expanded' cs = maybe (NodeInfo "" [] False []) id (lookup n fnodes)
                urlAttr = if not expanded' then [URL (TL.pack $ "http://localhost:3000/forceExpand?path=" ++ T.unpack (encodePath path'))] else []
                rootAttr = if path' == [0] then [Data.GraphViz.style filled, if expanded' then fillColor Yellow else fillColor GreenYellow, color Red] else []
                expAttr = if path' /= [0] && not expanded' then [Data.GraphViz.style filled, fillColor Green] else [] 
                attrs' = urlAttr ++ rootAttr ++ expAttr
                label' = Label . StrLabel $ TL.pack ("{ " ++ sanitise fNode
                         ++ (if null cs then "" else " | { " ++  ((List.intercalate "|" (map (\(x, eid) -> "<" ++ sanitise x ++ "--" ++ show eid ++ ">") (zip cs ([0..] :: [Int])))) ++ "}")) ++ "}")
            in node n $ [ label' , Shape Record ] ++ attrs'
        ) nodes
  mapM_ (\(a, b, eid) -> edge a b [TailPort $ LabelledPort (PN $ TL.pack (sanitise b ++ "--" ++ show eid)) Nothing]) edges
  where sanitise = map replace
        replace '{' = '['
        replace '}' = ']'
        replace c = c

graphvizProcess :: Data.GraphViz.Types.PrintDotRepr dg n =>
                         GraphvizCommand -> [Char] -> dg n -> IO ()
graphvizProcess comm outPath dotGraph = do
  let cmd = case comm of
              Dot ->  "dot"
              _ -> "sfdp"
      args = ["-Tsvg", "-o", outPath]
  let processSpec = (proc cmd args)
        { std_in = CreatePipe
        , std_err = NoStream
        }
  withCreateProcess processSpec $ \(Just hIn) _ _ ph -> do
    hSetBuffering hIn LineBuffering
    hPutStrLn hIn (TL.unpack $ printDotGraph dotGraph)
    hClose hIn
    _ <- waitForProcess ph
    return ()

noNodeOverlap :: DotGraph a -> DotGraph a 
noNodeOverlap g = g { graphStatements = (graphStatements g) <> stmts }
  where stmts = [ GA $ GraphAttrs [Overlap (PrismOverlap Nothing)] ] 
