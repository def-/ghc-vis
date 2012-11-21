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

import Control.Monad

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

foldlMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldlMaybe f a bs =
   foldr (\b g x -> f x b >>= g) Just bs a

-- | Take the objects to be visualized and run them through @dot@ and extract
--   the drawing operations that have to be exectued to show the graph of the
--   heap map.
xDotParse :: [(Box, String)] -> IO ([(Object Node, Operation)], [Box], Rectangle)
xDotParse as = do
  hm <- walkHeap as

  let nodes = zip [0..] $ map (\(_,(_,c)) -> c) rhm
      edges = do
        mbe <- foldM mbEdges [] nodes
        return $ foldlMaybe toLEdge [] mbe
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

      toLEdge xs (0, Just t) = if length rhm <= t
        then Nothing -- This might be able to happen, let's make sure it doesn't
        else case rhm !! t of
          (_,(Just name, _)) -> Just $ (0,t,name):xs
          (_,(Nothing, _))   -> Just $ (0,t,""):xs
      toLEdge xs (f, Just t) = Just $ (f,t,""):xs
      toLEdge xs _ = Just xs

      mbEdges xs (p,BCOClosure _ _ _ bPtr _ _ _) = do
        children <- bcoChildren [bPtr]
        return $ map (\b -> (p, Just b)) children ++ xs
      -- Using allPtrs and then filtering the closures not available in the
      -- heap map out emulates pointersToFollow without being in IO
      mbEdges xs (p,c) = return $ map (\b -> (p, boxPos b)) (allPtrs c) ++ xs

      boxPos :: Box -> Maybe Int
      boxPos b = lookup b $ zip (map fst rhm) [0..]

      bcoChildren :: [Box] -> IO [Int]
      bcoChildren [] = return []
      bcoChildren (b:bs) = case boxPos b of
        Nothing  -> do c <- getBoxedClosureData b
                       ptf <- pointersToFollow2 c
                       bcoChildren (ptf ++ bs)
        Just pos -> do children <- bcoChildren bs
                       return $ pos : children

  es' <- edges

  case es' of
    Nothing -> xDotParse as
    Just es -> do
      -- Convert a heap map, our internal data structure, to a graph that can be
      -- converted to a dot graph.
      let buildGraph :: Gr Closure String
          buildGraph = insEdges es $ insNodes nodes empty

      xDot <- graphvizWithHandle Dot (defaultVis $ toViewableGraph buildGraph) XDot hGetDot
      return (getOperations xDot, getBoxes hm, getSize xDot)

getBoxes :: HeapMap -> [Box]
getBoxes hm = map (\(b,(_,_)) -> b) $ reverse hm

-- Probably have to do some kind of fold over the graph to remove for example
-- unwanted pointers
toViewableGraph :: Gr Closure String -> Gr String String
toViewableGraph cg = emap id $ nmap showClosure cg

defaultVis :: (Graph gr) => gr String String -> DotGraph Node
defaultVis = graphToDot nonClusteredParams
  -- Somehow (X11Color Transparency) is white, use (RGBA 0 0 0 0) instead
  -- Ordering OutEdges is not strong enough to force edge ordering, might not look good anyway
  { globalAttributes = [GraphAttrs [BgColor [RGBA 0 0 0 0], FontName fontName, FontSize graphFontSize]]
  , fmtNode = \ (_,l) -> [toLabel l, FontName fontName, FontSize nodeFontSize]
  --, fmtNode = \ (_,l) -> [toLabel l, FontName fontName, FontSize nodeFontSize, Style [SItem Filled []], FillColor [RGBA 255 255 255 255], Color [RGBA 0 0 0 255]]
  --, fmtNode = \ (_,l) -> [toLabel l, FontName fontName, FontSize nodeFontSize, Shape PlainText]
  , fmtEdge = \ (_,_,l) -> [toLabel l, FontName fontName, FontSize edgeFontSize]
  }
