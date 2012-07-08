--module GHC.Vis.Graph (
--)
--where

import Control.Monad
import Data.Tuple
import Data.Maybe
import Data.Text.IO
import qualified Data.Text.Lazy as B

import Data.Graph.Inductive

import Data.GraphViz
import Data.GraphViz.Types
import qualified Data.GraphViz.Types.Generalised as G

import GHC.HeapView
import GHC.Vis

--buildGraph :: [Box] -> IO (Gr Closure ())
--buildGraph boxes = foldM go empty boxes
--  where go graph box@(Box content) = do
--          closure  <- getBoxedClosureData box
--          nodename <- getID content
--          edges <- mapM (\(Box x) -> getID x >>= \target -> return (nodename, target, ())) (pointersToFollow closure)
--          if nodename `gelem` graph
--            then return graph
--            else do
--                  graph' <- foldM go (insNode (nodename, closure) graph) $ pointersToFollow closure 
--                  return $ insEdges edges graph'

--graphToVisObjectList :: Gr Closure () -> [Box] -> [[VisObject]]
--graphToVisObjectList graph startnodes = mapM go startnodes
--  where go node = if indeg graph node <= 1
--                  then
--                  else NamedObject node (packVisObjects

--bg :: Gr String String
--bg = buildGr [([],1,"test1",[("",1)])
--             ,([],2,"test2",[("",3)])
--             ,([],3,"test3",[("",2)])
--             ]

-- Maybe add edge labels as type of link (params, pointer, ...)
buildGraph :: HeapMap -> Gr Closure ()
buildGraph hm = insEdges edges $ insNodes nodes empty
  where nodes = zip [0..] $ map (\(_,(_,c)) -> c) rhm
        edges = foldr toLEdge [] $ foldr mbEdges [] nodes
        -- Reversing it fixes the ordering of nodes in the graph. Should run
        -- through allPtrs and sort by order inside of all allPtrs lists.
        --
        -- When building the graph directly out of [Box] instead of going
        -- through the HeapMap, then the order of nodes might not be right for
        -- non-trivial graphs.
        --
        -- In some cases it's impossible to get the order right. Maybe there is
        -- a way in graphviz to specify outgoing edge orientation after all?
        rhm = reverse hm

        toLEdge (f, Just t) xs = (f,t,()):xs
        toLEdge _ xs = xs

        -- Using allPtrs and then filtering the closures not available in the
        -- heap map out emulates pointersToFollow without being in IO
        mbEdges (p,c) xs = (map (\b -> (p, boxPos b)) (allPtrs c)) ++ xs

        boxPos b = lookup b $ zip (map (\(b,_) -> b) rhm) [0..]

-- Probably have to do some kind of fold over the graph to remove for example unwanted pointers
toViewableGraph :: Gr Closure () -> Gr String String
toViewableGraph cg = emap (const "") $ nmap showClosure cg

--parseXDot :: FilePath -> IO (DotGraph String)
--parseXDot x = do
--  f <- readFile x
--  let fs = TL.pack f
--
--  return $ parseDotGraph fs

cyc3 :: Gr String String
cyc3 = buildGr -- cycle of three nodes
       [([("",3)],1,"test1",[("",2)]),
              ([],2,"test2",[("",3)]),
              ([],3,"test3",[])]

defaultVis :: (Graph gr) => gr String el -> DotGraph Node
--defaultVis = graphToDot (defaultParams :: GraphvizParams Node nl el String nl)
defaultVis = graphToDot params
  where params = nonClusteredParams { fmtNode = \ (_,l) -> [toLabel l] }
  -- Creates two clusters to get all initial values on a row; doesn't look very
  -- nice
  --where params = Params { isDirected = True
  --                      , globalAttributes = []
  --                      , clusterBy        = clustBy
  --                      , clusterID        = Int
  --                      , fmtCluster       = clFmt
  --                      , fmtNode = \(_,l) -> [toLabel l]
  --                      , fmtEdge = const []
  --                      }
  --      clustBy (n,l) = C (if l == "Blackhole" then 1 else 0) $ N (n,l)
  --      --clustBy (n,l) = C (n `mod` 2) $ N (n,l)
  --      clFmt 1 = [GraphAttrs [rank SameRank]]
  --      clFmt 0 = []

main = do
  --putStrLn $ show $ printDotGraph $ defaultVis cyc3
  hm <- walkHeapWithoutDummy [asBox 1]
  preview $ toViewableGraph $ buildGraph hm

pr as = do
  hm <- walkHeapWithoutDummy as
  preview $ toViewableGraph $ buildGraph hm

dg :: [Box] -> IO (DotGraph String)
dg as = do
  hm <- walkHeapWithoutDummy as
  xDotText <- graphvizWithHandle Dot (defaultVis $ toViewableGraph $ buildGraph hm) XDot hGetContents
  return $ parseDotGraph $ B.fromChunks [xDotText]
