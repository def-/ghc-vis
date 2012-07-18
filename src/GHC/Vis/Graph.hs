{-# LANGUAGE OverloadedStrings #-}

module GHC.Vis.Graph (
  dg,
  pr,
  op,
  getOperations,
  Operation(..),
  parse,
  drawAll
)
where

import Control.Monad
import Data.Tuple
import Data.Maybe
import Data.Char
import Data.Ratio
import Data.Text.IO
import qualified Data.Foldable as F
import qualified Data.Text.Lazy as B
import qualified Data.Text.Lazy.Read as B

import qualified Text.ParserCombinators.Poly.StateText as P

import Data.Graph.Inductive

import Data.GraphViz hiding (Ellipse, Polygon, parse)
import Data.GraphViz.Types hiding (parse)
import Data.GraphViz.Parsing hiding (parse)
import qualified Data.GraphViz.Attributes.Complete as A
import qualified Data.GraphViz.Types.Generalised as G

import GHC.HeapView
import GHC.Vis

import Graphics.UI.Gtk hiding (Box, Signal, Color, Alignment, Point, Dot, toLabel)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events as E

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

getBoxes :: HeapMap -> [Box]
getBoxes hm = map (\(b,(_,_)) -> b) $ reverse hm

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

dg :: [Box] -> IO (G.DotGraph Node, [Box])
dg as = do
  hm <- walkHeap as
  xDotText <- graphvizWithHandle Dot (defaultVis $ toViewableGraph $ buildGraph hm) XDot hGetContents
  return (parseDotGraph $ B.fromChunks [xDotText], getBoxes hm)

--drawGraph bs (G.DotGraph _ _ _ graphStatements) = do
--  mapM (drawGS bs) graphStatements
--
--drawGS bs (G.GA (GraphAttrs attrs)) = do
--  foldr parseDraw [] attrs
--
--drawGS bs (G.DN (DotNode id attrs)) = do
--  foldr parseDraw [] attrs
--
--drawGS bs (G.DE (DotEdge _ _ attrs)) = do
--  foldr parseDraw [] attrs

op as = do
  (dotGraph, boxes) <- dg as
  return (getOperations dotGraph, boxes, getSize dotGraph)

getSize :: (G.DotGraph Node) -> (Double, Double, Double, Double)
getSize (G.DotGraph _ _ _ graphStatements) = F.foldr handle (0,0,0,0) graphStatements
  where handle (G.GA (GraphAttrs attrs)) l = foldr handleInternal (0,0,0,0) attrs
        handle _ l = l

        handleInternal (A.BoundingBox (A.Rect (A.Point x y _ _) (A.Point w h _ _))) l = (x,y,w,h)
        handleInternal _ l = l

getOperations :: (G.DotGraph Node) -> [(Maybe Node, Operation)]
getOperations (G.DotGraph _ _ _ graphStatements) = F.foldr handle [] graphStatements
  where handle (G.GA (GraphAttrs attrs)) l = (zip (repeat Nothing) (foldr handleInternal [] attrs)) ++ l
        handle (G.DN (DotNode id attrs)) l = (zip (repeat $ Just id) (foldr handleInternal [] attrs)) ++ l
        handle (G.DE (DotEdge _ _ attrs)) l = (zip (repeat Nothing) (foldr handleInternal [] attrs)) ++ l
        handle _ l = l

        handleInternal (A.UnknownAttribute "_draw_" r) l = (parse r) ++ l
        handleInternal (A.UnknownAttribute "_ldraw_" r) l = (parse r) ++ l
        handleInternal (A.UnknownAttribute "_hdraw_" r) l = (parse r) ++ l
        handleInternal (A.UnknownAttribute "_tdraw_" r) l = (parse r) ++ l
        handleInternal (A.UnknownAttribute "_hldraw_" r) l = (parse r) ++ l
        handleInternal (A.UnknownAttribute "_tlldraw_" r) l = (parse r) ++ l
        handleInternal _ l = l

drawAll s (x,y,w,h) ops = do
  save
  scale 1 (-1)
  translate (-0.5 * w) (-0.5 * h)
  boundingBoxes <- mapM (draw s) ops
  restore
  --return $ concat boundingBoxes
  return $ map (\(o, (x,y,w,h)) -> (o, (x,y*(-1),w,h))) $ concat boundingBoxes

draw s (mn, Ellipse (x,y) w h filled) = do
  save
  translate x y
  scale w h
  moveTo 1 0
  arc 0 0 1 0 (2 * pi)
  restore
  case mn of
    Nothing -> setSourceRGB 0 0 0
    Just name -> case hover2 s of
      Just name2 -> if name == name2 then setSourceRGB 1 0 0 else setSourceRGB 0 0 0
      _ -> setSourceRGB 0 0 0
  if filled then fill else stroke
  setSourceRGB 0 0 0
  return $ case mn of
    Just node -> [(node, (x-w,y+h,2*w,2*h))]
    Nothing   -> []

draw s (mn, Polygon ((x,y):xys) filled) = do
  moveTo x y
  mapM (\(x,y) -> lineTo x y) xys
  closePath
  if filled then fillPreserve >> fill else stroke
  return []

draw s (mn, Polyline _) = return []

draw s (mn, BSpline ((x,y):xys) filled) = do
  moveTo x y
  drawBezier xys
  if filled then fillPreserve >> fill else stroke
  return []

  where drawBezier ((x1,y1):(x2,y2):(x3,y3):xys) = do
          curveTo x1 y1 x2 y2 x3 y3
          drawBezier xys
        drawBezier _ = return ()

-- TODO: Should be done with Pango
draw s (mn, Text (x,y) alignment width text) = do
  let x2 = case alignment of
             LeftAlign -> x
             CenterAlign -> x - 0.5 * width
             RightAlign -> x - width
  TextExtents _ _ _ height _ _ <- textExtents text
  let y2 = y - height + 2 -- TODO: proper descent from font metrics

  moveTo x2 y2
  save
  scale 1 (-1)
  showText text
  restore
  return []

draw s (mn, Color (r,g,b,a) filled) = do
  setSourceRGBA r g b a
  return []

-- TODO: Should be done with Pango
draw s (mn, Font size name) = do
  selectFontFace name FontSlantNormal FontWeightNormal
  setFontSize size
  --layout <- createLayout "test"
  return []

draw s (mn, Style _) = return []
draw s (mn, Image _ _ _ _) = return []

type Point = (Double, Double)

data Alignment = LeftAlign
               | CenterAlign
               | RightAlign
               deriving Show

-- http://www.graphviz.org/doc/info/output.html#d:xdot
data Operation = Ellipse { xy :: Point, w :: Double, h :: Double, filled :: Bool }
               | Polygon { points :: [Point], filled :: Bool }
               | Polyline { points :: [Point] }
               | BSpline { points :: [Point], filled :: Bool }
               | Text { baseline :: Point, alignment :: Alignment, width :: Double, text :: String }
               | Color { rgba :: (Double, Double, Double, Double), filled :: Bool }
               | Font { size :: Double, name :: String }
               | Style { style :: String }
               | Image { xy :: Point, w :: Double, h :: Double, name :: String }
               deriving Show

str1 = B.pack "c 9 -#000000ff e 63 167 27 19 "
str2 = B.pack "F 14.000000 11 -Times-Roman c 9 -#000000ff T 63 163 0 4 1 -: "
str3 = B.pack "e 1.00000 2.3 2 4 "

parse = Data.GraphViz.Parsing.runParser' $ P.many $ do
  t <- P.next
  character ' '

  case t of
    'E' -> parseEllipse True
    'e' -> parseEllipse False
    'P' -> parsePolygon True
    'p' -> parsePolygon False
    'L' -> parsePolyline
    'B' -> parseBSpline False
    'b' -> parseBSpline True
    'T' -> parseText
    'C' -> parseColor True
    'c' -> parseColor False
    'F' -> parseFont
    'S' -> parseStyle
    'I' -> parseImage
    _   -> fail "Unknown Operation"

parseEllipse filled = do
  p <- parsePoint
  (w,h) <- parsePoint
  return $ Ellipse p w h filled

parsePolygon filled = do
  xs <- parsePoints
  return $ Polygon xs filled

parsePolyline = parsePoints >>= return . Polyline

parseBSpline filled = do
  xs <- parsePoints
  return $ BSpline xs filled

parseText = do
  baseline <- parsePoint
  j <- parseInt'
  let alignment = case j of
                    -1 -> LeftAlign
                    0  -> CenterAlign
                    1  -> RightAlign
  character ' '
  width <- parseFloat'
  character ' '
  text <- parseString
  return $ Text baseline alignment width text

parseFont = do
  size <- parseFloat'
  character ' '
  name <- parseString
  return $ Font size name

parseStyle = parseString >>= return . Style

parseImage = do
  xy <- parsePoint
  (w,h) <- parsePoint
  name <- parseString
  return $ Image xy w h name

parseString = do
  n <- parseInt
  character ' '
  character '-'
  text <- sequence . replicate (fromInteger n) $ P.next
  character ' '
  return text

parsePoints = do
  n <- parseInt
  character ' '
  sequence . replicate (fromInteger n) $ parsePoint

parsePoint = do
  x <- parseFloat'
  character ' '
  y <- parseFloat'
  character ' '
  return (x,y)

parseColor filled = do -- Not complete
  n <- parseInt
  character ' '
  character '-'
  character '#'
  r <- parseHex
  g <- parseHex
  b <- parseHex
  a <- parseHex
  character ' '
  return $ Color (r,g,b,a) filled
 where parseHex = ((sequence . replicate 2) P.next) >>= return . hexToFloat
       hexToFloat s = (foldl (\x y -> 16 * x + fromIntegral (digitToInt y)) 0 s) / 255

parseSigned p = (character '-' >> liftM negate p)
                `P.onFail`
                p

parseInt = do cs <- P.many1Satisfy isDigit
              case B.decimal cs of
                Right (n,"")  -> return n
                Right (_,txt) -> fail $ "Trailing digits not parsed as Integral: " ++ B.unpack txt
                Left err      -> fail $ "Could not read Integral: " ++ err
           `P.adjustErr` ("Expected one or more digits\n\t"++)

parseInt' = parseSigned parseInt

parseFloat = do ds   <- P.manySatisfy isDigit
                frac <- P.optional
                        $ do character '.'
                             P.manySatisfy isDigit
                when (B.null ds && noDec frac)
                  (fail "No actual digits in floating point number!")
                expn  <- P.optional parseExp
                when (isNothing frac && isNothing expn)
                  (fail "This is an integer, not a floating point number!")
                let frac' = fromMaybe "" frac
                    expn' = fromMaybe 0 expn
                ( return . fromRational . (* (10^^(expn' - fromIntegral (B.length frac'))))
                  . (%1) . Data.GraphViz.Parsing.runParser' parseInt) (ds `B.append` frac')
             `P.onFail`
             fail "Expected a floating point number"
  where
    parseExp = do character 'e'
                  ((character '+' >> parseInt)
                   `P.onFail`
                   parseInt')
    noDec = maybe True B.null

parseFloat' = parseSigned ( parseFloat
                            `onFail`
                            liftM fI parseInt
                          )
  where
    fI :: Integer -> Double
    fI = fromIntegral
