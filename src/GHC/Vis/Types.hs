{-# LANGUAGE RankNTypes #-}
{- |
   Module      : GHC.Vis.Types
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

 -}
module GHC.Vis.Types (
  Point,
  DrawFunction,
  Signal(..),
  View(..),
  ViewType(..),
  State(..),
  Identifier,
  NamedBox,
  PState(..),
  PrintState,
  VisObject(..)
  )
  where

import GHC.HeapView

import qualified Control.Monad.State.Strict as MS

import Graphics.UI.Gtk hiding (Box, Signal, Point)
import Graphics.Rendering.Cairo hiding (x)

-- | A simple Point
type Point = (Double, Double)

-- | A function to draw a cairo drawing to a file.
type DrawFunction = forall a. FilePath -> Double -> Double -> (Surface -> IO a) -> IO a

-- | Signals that are sent to the GUI to signal changes
data Signal = NewSignal Box String -- ^ Add a new Box to be visualized
            | UpdateSignal         -- ^ Update the view
            | RedrawSignal         -- ^ Redraw
            | ClearSignal          -- ^ Remove all Boxes
            | RestoreSignal        -- ^ Reset all hidden boxes
            | SwitchSignal         -- ^ Switch to alternative view
            | HistorySignal (Int -> Int) -- ^ Change position in history
            | ExportSignal DrawFunction String -- ^ Export the view to a file

-- | All functions a view has to provide
data View = View
  { redraw        :: forall w. WidgetClass w => w -> Render ()
  , click         :: IO ()
  , move          :: forall w. WidgetClass w => w -> IO ()
  , updateObjects :: [NamedBox] -> IO ()
  , exportView    :: DrawFunction -> String -> IO ()
  }

-- | Visualization views
data ViewType = ListView
              | GraphView
              deriving (Enum)

-- | The global state used for the visualizations.
data State = State
  { mousePos   :: Point    -- ^ Current position of the mouse, updated with every movement
  , view       :: ViewType -- ^ What kind of visualization is currently running
  , zoomRatio  :: Double   -- ^ How much the window is zoomed in
  , position   :: Point    -- ^ Current position in the zoom
  , dragging   :: Bool     -- ^ Whether the mouse is dragging
  , wasDragged :: Bool     -- ^ Whether the mouse was actually dragged
  , heapDepth  :: Int      -- ^ Maximum heap depth to follow
  }

-- | Identifier of a closure
type Identifier = [String]

-- | A box and its identifier
type NamedBox = (Identifier, Box)

-- | Internal state of the list view generator
data PState = PState
  { tCounter' :: Integer
  , fCounter' :: Integer
  , xCounter' :: Integer
  , bindings  :: [HeapGraphIndex]
  , heapGraph :: HeapGraph String
  }

-- | The state of a printing operation
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
