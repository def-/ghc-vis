module GHC.Vis.Types (
  VisObject(..),
  State(..),
  HeapEntry,
  HeapMap,
  PrintState,
  Point,
  Alignment(..),
  Operation(..),
  Signal(..)
  )
  where

import GHC.HeapView

import Data.Graph.Inductive.Graph

import qualified Control.Monad.State as MS

data State = State
  { boxes :: [Box]
  , boxes2 :: [Box]
  , objects :: [[VisObject]]
  , objects2 :: ([(Maybe Node, Operation)], [Box], (Double, Double, Double, Double))
  , bounds :: [(String, (Double, Double, Double, Double))]
  , bounds2 :: [(Int, (Double, Double, Double, Double))]
  , mousePos :: (Double, Double)
  , hover :: Maybe String
  , hover2 :: Maybe Int
  , mode :: Bool
  }

type HeapEntry = (Maybe String, Closure)
-- We're using a slow, eq-based list instead of a proper map because
-- StableNames' hash values aren't stable enough
type HeapMap   = [(Box, HeapEntry)]
-- The second HeapMap includes BCO pointers, needed for list visualization
type PrintState = MS.State (Integer, HeapMap, HeapMap)

data VisObject = Unnamed String
               | Named String [VisObject]
               | Link String
               | Function String
               deriving Eq

instance Show VisObject where
  show (Unnamed x) = x
  show (Named x ys) = x ++ "=(" ++ show ys ++ ")"
  show (Link x) = x
  show (Function x) = x

  showList []       = showString ""
  showList (c:cs)   = showString (show c) . showList cs

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

data Signal = NewSignal Box String -- Add a new Box to be visualized
            | UpdateSignal  -- Redraw
            | ClearSignal   -- Remove all Boxes
            | SwitchSignal  -- Switch to alternative view
