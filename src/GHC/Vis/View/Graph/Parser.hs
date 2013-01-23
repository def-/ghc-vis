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

import Data.List
import Data.Maybe
import Data.Functor
import Control.Arrow

import qualified Data.Text.Lazy as B

import Data.Graph.Inductive hiding (nodes, edges)

import Data.GraphViz hiding (Ellipse, Polygon, parse)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Commands.IO

import GHC.HeapView hiding (name)
import GHC.Vis.Internal
import GHC.Vis.Types

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

-- | Take the objects to be visualized and run them through @dot@ and extract
--   the drawing operations that have to be exectued to show the graph of the
--   heap map.
xDotParse :: [(Box, String)] -> IO ([(Object Node, Operation)], [Box], Rectangle)
xDotParse as = do
  hm <- walkHeap as

  let -- Reversing it fixes the ordering of nodes in the graph. Should run
      -- through allPtrs and sort by order inside of all allPtrs lists.
      --
      -- When building the graph directly out of [Box] instead of going
      -- through the HeapMap, then the order of nodes might not be right for
      -- non-trivial graphs.
      --
      -- In some cases it's impossible to get the order right. Maybe there is
      -- a way in graphviz to specify outgoing edge orientation after all?
      rhm = reverse hm

      toLNode (_,(_,c@BCOClosure {bcoptrs = bPtr})) = do
        children <- bcoChildren [bPtr]
        return (c, length children)
      toLNode (_,(_,c)) = do
        return (c, length (allPtrs c))

      toLEdge (0, Just t, i) = if length rhm <= t
        then Nothing -- This might be able to happen, let's make sure it doesn't
        else case rhm !! t of
          (_,(Just name, _)) -> Just (0,t,(name, i))
          (_,(Nothing, _))   -> Just (0,t,("",i))
      toLEdge (f, Just t, i) = Just (f,t,("",i))
      toLEdge _ = Nothing

      mbEdges (p,(BCOClosure _ _ _ bPtr _ _ _,_)) = do
        children <- bcoChildren [bPtr]
        return $ zipWith (\b i -> (p, Just b, i)) children [0..]
      -- Using allPtrs and then filtering the closures not available in the
      -- heap map out emulates pointersToFollow without being in IO
      mbEdges (p,(c,_)) = return $ zipWith (\b i -> (p, boxPos b, i)) (allPtrs c) [0..]

      boxPos :: Box -> Maybe Int
      boxPos b = elemIndex b $ map fst rhm

      bcoChildren :: [Box] -> IO [Int]
      bcoChildren [] = return []
      bcoChildren (b:bs) = case boxPos b of
        Nothing  -> do c <- getBoxedClosureData b
                       ptf <- pointersToFollow2 c
                       bcoChildren (ptf ++ bs)
        Just pos -> do children <- bcoChildren bs
                       return $ pos : children

  nodes <- zip [0..] <$> mapM toLNode rhm
  mbe <- concat <$> mapM mbEdges nodes
  let edges = mapMaybe toLEdge mbe

  -- Convert a heap map, our internal data structure, to a graph that can be
  -- converted to a dot graph.
  let buildGraph :: Gr (Closure, Int) (String, Int)
      buildGraph = insEdges edges $ insNodes nodes empty

  xDot <- graphvizWithHandle Dot (defaultVis $ toViewableGraph buildGraph) XDot hGetDot
  return (getOperations xDot, getBoxes hm, getSize xDot)

getBoxes :: HeapMap -> [Box]
getBoxes hm = map (\(b,(_,_)) -> b) $ reverse hm

-- Probably have to do some kind of fold over the graph to remove for example
-- unwanted pointers
toViewableGraph :: Gr (Closure, Int) (String, Int) -> Gr (String, Int) (String, Int)
toViewableGraph cg = emap id $ nmap (first showClosure) cg

defaultVis :: (Graph gr) => gr (String, Int) (String, Int)-> DotGraph Node
defaultVis = graphToDot nonClusteredParams
  -- Somehow (X11Color Transparency) is white, use (RGBA 0 0 0 0) instead
  -- Ordering OutEdges is not strong enough to force edge ordering, might not look good anyway
  { globalAttributes = [GraphAttrs [BgColor [RGBA 0 0 0 0], FontName fontName, FontSize graphFontSize]]
  , fmtNode = \ (_,(l,i)) -> [toLabel l, FontName fontName, FontSize nodeFontSize]
  --, fmtNode = \ (_,l) -> [toLabel l, FontName fontName, FontSize nodeFontSize, Style [SItem Filled []], FillColor [RGBA 255 255 255 255], Color [RGBA 0 0 0 255]]
  --, fmtNode = \ (_,l) -> [toLabel l, FontName fontName, FontSize nodeFontSize, Shape PlainText]
  , fmtEdge = \ (_,_,(l,i)) -> [toLabel l, FontName fontName, FontSize edgeFontSize]
  }
