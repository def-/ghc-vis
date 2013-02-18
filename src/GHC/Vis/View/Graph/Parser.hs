{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : GHC.Vis.View.Graph.Parser
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

 -}
module GHC.Vis.View.Graph.Parser (
  xDotParse
)
where

import Data.Maybe
import Data.List

import qualified Data.IntMap as M

import qualified Data.Text.Lazy as B

import Data.Graph.Inductive hiding (nodes, edges, newNodes)

import Data.GraphViz hiding (Ellipse, Polygon, parse)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Commands.IO

import GHC.HeapView hiding (name)
import GHC.Vis.Internal

import Graphics.XDot.Types hiding (name, h, Style, Color)
import Graphics.XDot.Parser

fontName :: B.Text
--fontName = "Times Roman"
--fontName = "Helvetica"
fontName = "Sans"

graphFontSize :: Double
graphFontSize = 24

nodeFontSize :: Double
nodeFontSize = 24

edgeFontSize :: Double
edgeFontSize = 24

graphvizCommand :: GraphvizCommand
graphvizCommand = Dot

-- | Take the objects to be visualized and run them through @dot@ and extract
--   the drawing operations that have to be exectued to show the graph of the
--   heap map.
xDotParse :: [(Box, String)] -> IO ([(Object Node, Operation)], [WeakBox], [(Object Node, Rectangle)], Rectangle)
xDotParse as = do
  (HeapGraph hg, is) <- multiBuildHeapGraph 100 $ map fst as

  let hgList = M.toList hg

  let nodes = map (\(i,e@(HeapGraphEntry _ c)) -> (i, (e, length $ allPtrs c))) hgList
  let edges = map (\(x,(y,z)) -> (x,z,("",y))) $ concat $ map (\(i, (HeapGraphEntry _ c)) -> zip (repeat i) (zip [0..] $ map fromJust $ filter isJust $ allPtrs c)) hgList

  -- Convert a heap graph, our internal data structure, to a graph that can be
  -- converted to a dot graph.
  let buildGraph :: Gr (HeapGraphEntry, Int) (String, Int)
      buildGraph = insEdges edges $ insNodes nodes empty

  let newNodes = zip [-length as..] $ map (\(_,n) -> ([n], 0)) as
  let newEdges = map (\(_,(b,n)) -> ((fromJust $ findIndex (\(a,_) -> a == b) as) - length as, n, (fromJust $ lookup b as, 0))) $ zip [-length as..] is

  let insertMore gr = insEdges newEdges $ insNodes newNodes gr

  xDot <- graphvizWithHandle graphvizCommand (defaultVis $ insertMore $ toViewableGraph buildGraph) XDot hGetDot

  return (getOperations xDot, getBoxes (HeapGraph hg), getDimensions xDot, getSize xDot)

getBoxes :: HeapGraph -> [WeakBox]
getBoxes (HeapGraph hg) = map (\(HeapGraphEntry b _) -> b) $ M.elems hg

-- Probably have to do some kind of fold over the graph to remove for example
-- unwanted pointers
toViewableGraph :: Gr (HeapGraphEntry, Int) (String, Int) -> Gr ([String], Int) (String, Int)
toViewableGraph cg = emap id $ nmap (\((HeapGraphEntry _ c), i) -> (showClosureFields c, i)) cg

defaultVis :: (Graph gr) => gr ([String], Int) (String, Int) -> DotGraph Node
defaultVis = graphToDot nonClusteredParams
  -- Somehow (X11Color Transparency) is white, use (RGBA 0 0 0 0) instead
  -- Ordering OutEdges is not strong enough to force edge ordering, might not look good anyway
  { globalAttributes = [GraphAttrs [BgColor [RGBA 0 0 0 0], FontName fontName, FontSize graphFontSize]]
  , fmtNode = \(x,(l,i)) -> if x >= 0 then [
        --xLabel (B.pack "foo"),
        nodeLabel l i,
        Shape Record, FontName fontName, FontSize nodeFontSize]
      -- x < 0: Invisible marker nodes
      else [Shape PointShape, Style [SItem Invisible []]]
  --, fmtNode = \ (_,l) -> [toLabel l, FontName fontName, FontSize nodeFontSize, Style [SItem Filled []], FillColor [RGBA 255 255 255 255], Color [RGBA 0 0 0 255]]
  --, fmtNode = \ (_,l) -> [toLabel l, FontName fontName, FontSize nodeFontSize, Shape PlainText]
  , fmtEdge = \ (_,_,(l,i)) -> [
        TailPort (LabelledPort (PN (B.pack (show i))) Nothing),
        toLabel l,
        FontName fontName, FontSize edgeFontSize]
  }
  where
    {-
    All pointer origins below the constructor:
    nodeLabel l 0 = Label $ RecordLabel [FieldLabel (B.pack l) | l <- ls]
    nodeLabel l i = Label $ RecordLabel [
            FlipFields [
                FlipFields [FieldLabel (B.pack l) | l <- ls] ++
                FlipFields [PortName (PN (B.pack (show j))) | j <- [0..i-1]]
            ]]
    -}
    {-
    All pointer following the constructor on the right:
    -}
    nodeLabel ls i = Label $ RecordLabel $
                [FieldLabel (B.pack l) | l <- ls] ++
                [ PortName (PN (B.pack (show j))) | j <- [0..i-1]]
