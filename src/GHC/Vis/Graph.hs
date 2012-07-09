{-# LANGUAGE OverloadedStrings #-}

--module GHC.Vis.Graph (
--)
--where

import Control.Monad
import Data.Tuple
import Data.Maybe
import Data.Char
import Data.Ratio
import Data.Text.IO
import qualified Data.Text.Lazy as B
import qualified Data.Text.Lazy.Read as B

import qualified Text.ParserCombinators.Poly.StateText as P

import Data.Graph.Inductive

import Data.GraphViz hiding (Ellipse)
import Data.GraphViz.Types
import Data.GraphViz.Parsing
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

type Point = (Double, Double)

data Alignment = LeftAlign
               | CenterAlign
               | RightAlign
               deriving Show

-- http://www.graphviz.org/doc/info/output.html#d:xdot
data Operation = Ellipse { xy :: Point, w :: Double, h :: Double, filled :: Bool }
               | Polygon { points :: [Point], filled :: Bool }
               | BSpline { points :: [Point], filled :: Bool }
               | Text { baseline :: Point, alignment :: Alignment, width :: Double, text :: String }
               | Color { color :: String, filled :: Bool }
               | Font { size :: Double, name :: String }
               | Style { style :: String }
               | Image { xy :: Point, w :: Double, h :: Double, name :: String }
               deriving Show

parse = P.runParser p [] str3
  where str1 = B.pack "c 9 -#000000ff e 63 167 27 19 "
        str2 = B.pack "F 14.000000 11 -Times-Roman c 9 -#000000ff T 63 163 0 4 1 -: "
        str3 = B.pack "e 1.00000 2.3 2 4"
        p = do
          t <- P.next
          s <- P.next
          r <- case s of
            ' ' -> case t of
                     'E' -> parseEllipse True
                     'e' -> parseEllipse False
                     _   -> fail "Unknown Operation"
            _   -> fail "Space missing"
          P.next
          return r
        parseEllipse filled = do
          x <- P.manySatisfy (/= ' ')
          P.next
          y <- P.manySatisfy (/= ' ')
          P.next
          w <- P.manySatisfy (/= ' ')
          P.next
          h <- P.manySatisfy (/= ' ')
          return $ Ellipse (0,0) 0 0 filled

runParser' p = checkValidParse . fst . P.runParser p'
  where
    p' = p `discard` (whitespace >> P.eof)

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
                  . (%1) . P.runParser parseInt) (ds `B.append` frac')
             `P.onFail`
             fail "Expected a floating point number"
  where
    parseExp = do character 'e'
                  ((character '+' >> parseInt)
                   `P.onFail`
                   parseInt')
    noDec = maybe True B.null
