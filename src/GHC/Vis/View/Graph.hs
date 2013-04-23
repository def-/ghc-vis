{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables #-}
{- |
   Module      : GHC.Vis.View.Graph
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

 -}
module GHC.Vis.View.Graph (
  export,
  redraw,
  click,
  rightClick,
  move,
  updateObjects
  )
  where

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif

import Graphics.UI.Gtk hiding (Box, Signal, Rectangle, Object)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo

import Control.Concurrent
import Control.Monad
import Control.Exception

import Data.IORef
import System.IO.Unsafe

import GHC.Vis.View.Graph.Parser
import GHC.Vis.Types hiding (State, View(..))
import GHC.Vis.View.Common

import GHC.HeapView hiding (size)

import Graphics.XDot.Viewer
import Graphics.XDot.Types hiding (size, w, h)

data State = State
  { boxes      :: [Box]
  , operations :: [(Object Int, Operation)]
  , totalSize  :: (Double, Double, Double, Double)
  , bounds     :: [(Object Int, (Double, Double, Double, Double))]
  , hover      :: Object Int
  }

state :: IORef State
state = unsafePerformIO $ newIORef $ State [] [] (0, 0, 1, 1) [] None

-- | Draw visualization to screen, called on every update or when it's
--   requested from outside the program.
redraw :: WidgetClass w => w -> IO ()
redraw canvas = do
  s <- readIORef state
  Gtk.Rectangle _ _ rw2 rh2 <- widgetGetAllocation canvas

  boundingBoxes <- render canvas (draw s rw2 rh2)

  modifyIORef state (\s' -> s' {bounds = boundingBoxes})

-- | Export the visualization to an SVG file
export :: DrawFunction -> String -> IO ()
export drawFn file = do
  s <- readIORef state

  let (_, _, xSize, ySize) = totalSize s

  drawFn file xSize ySize
    (\surface -> renderWith surface (draw s (round xSize) (round ySize)))

  return ()

draw :: State -> Int -> Int -> Render [(Object Int, Rectangle)]
draw s rw2 rh2 = do
  if null $ boxes s then return []
  else do
    vS <- liftIO $ readIORef visState

    -- Line widths don't count to size, let's add a bit
    let rw = 0.97 * fromIntegral rw2
        rh = 0.97 * fromIntegral rh2

        ops = operations s
        size@(_,_,sw,sh) = totalSize s

    -- Proportional scaling
        (sx,sy) = (min 1000 $ zoomRatio vS * min (rw / sw) (rh / sh), sx)
        (ox1,oy1) = (0.5 * fromIntegral rw2, 0.5 * fromIntegral rh2)
        (ox2,oy2) = position vS
        (ox,oy) = (ox1 + ox2, oy1 + oy2)

    translate ox oy
    scale sx sy

    result <- drawAll (hover s) size ops

    return $ map (\(o, (x,y,w,h)) -> (o,
      ( x * sx + ox -- Transformations to correct scaling and offset
      , y * sy + oy
      , w * sx
      , h * sy
      ))) result

render :: WidgetClass w => w -> Render b -> IO b
render canvas r = do
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win r

-- | Handle a mouse click. If an object was clicked an 'UpdateSignal' is sent
--   that causes the object to be evaluated and the screen to be updated.
click :: IO ()
click = do
  s <- readIORef state

  hm <- inHistoryMode
  when (not hm) $ case hover s of
    -- This might fail when a click occurs during an update
    Node t -> unless (length (boxes s) <= t) $ do
      evaluate2 $ boxes s !! t
      -- Without forkIO it would hang indefinitely if some action is currently
      -- executed
      void $ forkIO $ putMVar visSignal UpdateSignal
    _ -> return ()

rightClick :: IO ()
rightClick = do
  s <- readIORef state

  case hover s of
    Node t -> unless (length (boxes s) <= t) $ do
      hide $ boxes s !! t
      void $ forkIO $ putMVar visSignal RedrawSignal
    _ -> return ()

evaluate2 :: Box -> IO ()
evaluate2 b@(Box a) = do
  c <- getBoxedClosureData b
  case c of
    -- ghc: internal error: MUT_ARR_PTRS_FROZEN object entered!
    -- (GHC version 7.4.2 for x86_64_unknown_linux)
    -- Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
    --ArrWordsClosure _ _ _ -> return () -- Don't inspect ArrWords
    --MutArrClosure _ _ _ _ -> return ()
    --MVarClosure _ _ _ _ -> return ()
    --_ -> a `seq` return ()
    IndClosure{} -> a `seq` return ()
    BlackholeClosure{} -> a `seq` return ()
    FunClosure{} -> a `seq` return ()
    ThunkClosure{} -> a `seq` return ()
    APClosure{} -> a `seq` return ()
    PAPClosure{} -> a `seq` return ()
    _ -> return ()
  `catch`
    \(e :: SomeException) -> putStrLn $ "Caught exception while evaluating: " ++ show e

hide :: Box -> IO ()
hide b = modifyMVar_ visHidden (\hs -> return $ b : hs)

-- | Handle a mouse move. Causes an 'UpdateSignal' if the mouse is hovering a
--   different object now, so the object gets highlighted and the screen
--   updated.
move :: WidgetClass w => w -> IO ()
move canvas = do
  vs <- readIORef visState
  oldS <- readIORef state
  let oldHover = hover oldS

  -- TODO: Evaluation and Collapsing menu here

  modifyIORef state $ \s' -> (
    let (mx, my) = mousePos vs
        check (o, (x,y,w,h)) =
          if x <= mx && mx <= x + w &&
             y <= my && my <= y + h
          then o else None

        validOne (None:xs) = validOne xs
        validOne (x:_) = x
        validOne _ = None
    in s' {hover = validOne $ map check (bounds oldS)}
    )
  s <- readIORef state
  unless (oldHover == hover s) $ widgetQueueDraw canvas

-- | Something might have changed on the heap, update the view.
updateObjects :: [NamedBox] -> IO ()
updateObjects _boxes = do
  hidden <- readMVar visHidden
  (ops, bs', _ , size) <- xDotParse $ hidden

  modifyIORef state (\s -> s {operations = ops, boxes = bs', totalSize = size, hover = None})
