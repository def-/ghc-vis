module GHC.Vis.GTK.Graph (
  redraw,
  click,
  tick,
  updateObjects
  )
  where
import Graphics.UI.Gtk hiding (Box, Signal)
import Graphics.Rendering.Cairo

import Control.Concurrent
import Control.Monad

import Data.IORef

import GHC.Vis.Internal hiding (boxes)
import GHC.Vis.Graph
import GHC.Vis.Types hiding (boxes)
import GHC.Vis.GTK.Common

import GHC.HeapView hiding (size)

import Graphics.XDot.Viewer

redraw :: WidgetClass w => w -> IO ()
redraw canvas = do
  s <- readIORef visState
  Rectangle _ _ rw rh <- widgetGetAllocation canvas

  let (ops, boxes', size@(_,_,sw,sh)) = objects2 s

  boundingBoxes <- render canvas $ do
    -- Proportional scaling
    let scalex = min (fromIntegral rw / sw) (fromIntegral rh / sh)
        scaley = scalex
        offsetx = 0.5 * fromIntegral rw
        offsety = 0.5 * fromIntegral rh
    save
    translate offsetx offsety
    scale scalex scaley

    result <- drawAll (hover2 s) size ops

    restore
    return $ map (\(o, (x,y,w,h)) -> (o, (x*scalex+offsetx,y*scaley+offsety,w*scalex,h*scaley))) result

  modifyIORef visState (\s' -> s' {boxes2 = boxes'})
  modifyIORef visState (\s' -> s' {bounds2 = boundingBoxes})

render :: WidgetClass w => w -> Render b -> IO b
render canvas r = do
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win r

  --Rectangle _ _ rw rh <- widgetGetAllocation canvas
  --withSVGSurface "export.svg" (fromIntegral rw) (fromIntegral rh) (\s -> renderWith s r)

click :: IO ()
click = do
  s <- readIORef visState

  case hover2 s of
    Just t -> do
      evaluate2 $ boxes2 s !! t
      putMVar visSignal UpdateSignal
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

tick :: WidgetClass w => w -> IO ()
tick canvas = do
  oldS <- readIORef visState
  let oldHover2 = hover2 oldS

  modifyIORef visState $ \s' -> (
    let (mx, my) = mousePos s'
        check (o, (x,y,w,h)) =
          if x <= mx && mx <= x + w &&
             y <= my && my <= y + h
          then Just o else Nothing
    in s' {hover2 = msum $ map check (bounds2 s')}
    )
  s <- readIORef visState
  unless (oldHover2 == hover2 s) $ widgetQueueDraw canvas

updateObjects :: [(Box, String)] -> IO ()
updateObjects boxes = do
  objs2 <- op boxes
  modifyIORef visState (\s -> s {objects2 = objs2})
