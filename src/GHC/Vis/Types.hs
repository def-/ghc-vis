{-# LANGUAGE RankNTypes #-}
{- |
   Module      : GHC.Vis.Types
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

 -}
module GHC.Vis.Types (
  DrawFunction,
  Signal(..),
  View(..),
  ViewType(..),
  State(..),
  HeapEntry,
  HeapMap,
  PState(..),
  PrintState,
  VisObject(..)
  )
  where

import GHC.HeapView

import qualified Control.Monad.State as MS

import Graphics.UI.Gtk hiding (Box, Signal, Point)
import Graphics.Rendering.Cairo

type Point = (Double, Double)

-- | A function to draw a cairo drawing to a file.
type DrawFunction = forall a. FilePath -> Double -> Double -> (Surface -> IO a) -> IO a

-- | Signals that are sent to the GUI to signal changes
data Signal = NewSignal Box String -- ^ Add a new Box to be visualized
            | UpdateSignal         -- ^ Redraw
            | ClearSignal          -- ^ Remove all Boxes
            | SwitchSignal         -- ^ Switch to alternative view
            | ExportSignal DrawFunction String -- ^ Export the view to a file

-- | All functions a view has to provide
data View = View
  { redraw        :: WidgetClass w => w -> IO ()
  , click         :: IO ()
  , move          :: WidgetClass w => w -> IO ()
  , updateObjects :: [(Box, String)] -> IO ()
  , exportView    :: DrawFunction -> String -> IO ()
  }

-- | Visualization views
data ViewType = ListView
              | GraphView
              deriving (Enum)

-- | The global state used for the visualizations.
data State = State
  { mousePos  :: Point    -- ^ Current position of the mouse, updated with every movement
  , view      :: ViewType -- ^ What kind of visualization is currently running
  , zoomRatio :: Double   -- ^ How much the window is zoomed in
  , position  :: Point    -- ^ Current position in the zoom
  , realPos   :: Point    -- ^ Current position in the zoom
  , dragging  :: Bool     -- ^ Whether the mouse is dragging
  }

-- | An entry in a 'HeapMap', consisting of an identifier and a parsed GHC heap entry
type HeapEntry =
  ( Maybe String
  , Closure
  )
-- | A map of heap objects.
--   We're using a slow, eq-based list instead of a proper map because
--   StableNames' hash values aren't stable enough
type HeapMap   = [(Box, HeapEntry)]

-- | The second HeapMap includes BCO pointers, needed for list visualization
data PState = PState
  { tCounter :: Integer
  , fCounter :: Integer
  , bCounter :: Integer
  , heapMap  :: HeapMap
  , heapMap' :: HeapMap
  }

type PrintState = MS.State PState

-- | Simple representation of objects on the heap, so they can be arranged linearly
data VisObject = Unnamed String
               | Named String [VisObject]
               | Link String
               | Thunk String
               | Function String
               deriving Eq

instance Show VisObject where
  show (Unnamed x)  = x
  show (Named x ys) = x ++ "=(" ++ show ys ++ ")"
  show (Link x)     = x
  show (Thunk x)    = x
  show (Function x) = x

  showList = foldr ((.) . showString . show) (showString "")
