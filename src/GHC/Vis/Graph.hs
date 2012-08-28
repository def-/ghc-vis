{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : GHC.Vis.Graph
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

 -}
module GHC.Vis.Graph (
  xDotParse
)
where

import System.IO.Unsafe

import Data.Text.IO
import qualified Data.Text.Lazy as B

import Data.Graph.Inductive hiding (nodes, edges)

import Data.GraphViz hiding (Ellipse, Polygon, parse)
import qualified Data.GraphViz.Types.Generalised as G

import GHC.HeapView hiding (name)
import GHC.Vis.Internal
import GHC.Vis.Types

import Graphics.XDot.Types hiding (name, h)
import Graphics.XDot.Parser

-- | Take the objects to be visualized and run them through @dot@ and extract
--   the drawing operations that have to be exectued to show the graph of the
--   heap map.
xDotParse :: [(Box, String)] -> IO ([(Maybe Node, Operation)], [Box], Rectangle)
xDotParse as = do
  (dotGraph, boxes) <- dg as
  return (getOperations dotGraph, boxes, getSize dotGraph)

dg :: [(Box, String)] -> IO (G.DotGraph Node, [Box])
dg as = do
  hm <- walkHeap as
  --hm <- walkHeapDepth as
  xDotText <- graphvizWithHandle Dot (defaultVis $ toViewableGraph $ buildGraph hm) XDot hGetContents
  return (parseDotGraph $ B.fromChunks [xDotText], getBoxes hm)

buildGraph :: HeapMap -> Gr Closure String
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

        toLEdge (0, Just t) xs = case rhm !! t of
          (_,(Just name, _)) -> (0,t,name):xs
          (_,(Nothing, _))   -> (0,t,""):xs
        toLEdge (f, Just t) xs = (f,t,""):xs
        toLEdge _ xs = xs

        mbEdges (p,BCOClosure _ _ _ bPtr _ _ _) xs = map (\b -> (p, Just b)) (bcoChildren [bPtr] hm) ++ xs
        -- Using allPtrs and then filtering the closures not available in the
        -- heap map out emulates pointersToFollow without being in IO
        mbEdges (p,c) xs = map (\b -> (p, boxPos b)) (allPtrs c) ++ xs

        boxPos :: Box -> Maybe Int
        boxPos b = lookup b $ zip (map fst rhm) [0..]

        bcoChildren :: [Box] -> HeapMap -> [Int]
        bcoChildren [] _ = []
        bcoChildren (b:bs) h = case boxPos b of
          Nothing  -> let ptf = unsafePerformIO $ getBoxedClosureData b >>= pointersToFollow2
                      in bcoChildren (ptf ++ bs) h -- Could go into infinite loop
          Just pos -> pos : bcoChildren bs h

getBoxes :: HeapMap -> [Box]
getBoxes hm = map (\(b,(_,_)) -> b) $ reverse hm

-- Probably have to do some kind of fold over the graph to remove for example
-- unwanted pointers
toViewableGraph :: Gr Closure String -> Gr String String
toViewableGraph cg = emap id $ nmap showClosure cg

defaultVis :: (Graph gr) => gr String String -> DotGraph Node
defaultVis = graphToDot params
  where params = nonClusteredParams { fmtNode = \ (_,l) -> [toLabel l], fmtEdge = \ (_,_,l) -> [toLabel l] }
