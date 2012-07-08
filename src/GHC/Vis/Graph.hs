--module GHC.Vis.Graph (
--)
--where

import Control.Monad
import Data.Tuple
import Data.Maybe
import qualified Data.Text.Lazy as TL

import Data.Graph.Inductive

import Data.GraphViz.Types
import Data.GraphViz
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

buildGraph :: HeapMap -> Gr Closure ()
buildGraph hm = insEdges edges $ insNodes nodes empty
  where nodes = zip [0..] $ map (\(_,(_,c)) -> c) hm
        edges = foldr toLEdge [] $ foldr mbEdges [] nodes

        toLEdge (f, Just t) xs = (f,t,()):xs
        toLEdge _ xs = xs

        mbEdges (p,c) xs = (map (\b -> (p, boxPos b)) (allPtrs c)) ++ xs

        boxPos b = lookup b $ zip (map (\(b,_) -> b) hm) [0..]

parseXDot :: FilePath -> IO (DotGraph String)
parseXDot x = do
  f <- readFile x
  let fs = TL.pack f

  return $ parseDotGraph fs

cyc3 :: Gr String String
cyc3 = buildGr -- cycle of three nodes
       [([("",3)],1,"test1",[("",2)]),
              ([],2,"test2",[("",3)]),
              ([],3,"test3",[])]

defaultVis :: (Graph gr, Labellable nl) => gr nl el -> DotGraph Node
--defaultVis = graphToDot (defaultParams :: GraphvizParams Node nl el String nl)
defaultVis = graphToDot params
  where params = nonClusteredParams { fmtNode = \ (_,l) -> [toLabel l] }

main = do
  putStrLn $ show $ printDotGraph $ defaultVis cyc3
