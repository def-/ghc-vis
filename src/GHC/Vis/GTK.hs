module GHC.Vis.GTK (
  tvis,
  vis,
  Signal(..),
  visGlobalVar
  )
  where
import Graphics.UI.Gtk hiding (Box, Signal)
import Graphics.Rendering.Cairo

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Monad

import System.IO.Unsafe

import GHC.Vis
import GHC.HeapView

fontSize = 15

visGlobalVar = unsafePerformIO (Control.Concurrent.MVar.newEmptyMVar :: IO (Control.Concurrent.MVar.MVar GHC.Vis.GTK.Signal))

data Signal = NewSignal Box
            | UpdateSignal
            | ClearSignal

tvis ref = do
  forkIO $ vis ref

vis ref = do
  bref <- newMVar [] :: IO (MVar [Box])

  initGUI
  window <- windowNew
  onDestroy window mainQuit

  canvas <- drawingAreaNew

  set window [ windowTitle := "Vis"
             , containerChild := canvas
             ]

  onExpose canvas $ const $ do
    redraw canvas bref
    return True

  widgetShowAll window

  forkIO $ react ref bref canvas

  mainGUI

react ref bref canvas = do
  signal <- takeMVar ref
  case signal of
    NewSignal x  -> modifyMVar_ bref (\y -> return $ y ++ [x])
    ClearSignal  -> modifyMVar_ bref (\_ -> return [])
    UpdateSignal -> putStrLn "update" >> return ()

  threadDelay 10000 -- 10 ms, else sometimes redraw happens too fast (Why?)
  widgetQueueDraw canvas
  react ref bref canvas

redraw canvas bref = do
  boxes <- readMVar bref
  heapMap <- mWalkHeap boxes
  --texts <- mapM (\(Box a) -> bprint a) boxes
  render canvas $ do
    mapM (drawEntry heapMap) boxes
    --setSourceRGB 0 0 0
    --moveTo 10 10
    --showText $ show texts

render canvas r = do
        win <- widgetGetDrawWindow canvas
        renderWithDrawable win $ do
          selectFontFace "DejaVu Sans" FontSlantNormal FontWeightNormal
          setFontSize fontSize
          r

drawEntry heapMap box = do
  let text = show box
  TextExtents xb yb w h _ _ <- textExtents text
  translate 100 30
  setSourceRGB 0 0 0
  moveTo (-w/2) (h/2)
  showText text

drawLinkObject heapMap box = do
  let text = "t1"
  let padding = 5
  TextExtents xb yb w h _ _ <- textExtents text

  translate 100 30

  let (ux, uy, uw, uh) =
        ( -w/2 + xb - padding
        ,  h/2 + yb - padding
        ,  w   +  2 * padding
        ,  h   +  2 * padding
        )

  setLineCap LineCapRound
  roundedRect ux uy uw uh
  setSourceRGB 1 0 0
  fillPreserve
  setSourceRGB 0 0 0
  stroke

  moveTo (-w/2) (h/2)
  showText text

drawNamedObject heapMap box = do
  let text = show box
  let text1 = "t1"
  let padding = 5
  TextExtents xb yb w h _ _ <- textExtents text
  TextExtents xb1 yb1 w1 h1 _ _ <- textExtents text1

  translate 100 30

  let (ux, uy, uw, uh) =
        ( -(max w w1)/2 + xb - 1.5 * padding
        ,  (h + h1)/2   + yb - 1.5 * padding
        ,  (max w w1)   +  3 * padding
        ,  (h + h1)     +  3 * padding
        )

  --setLineWidth 10
  setLineCap LineCapRound
  roundedRect ux uy uw uh
  setSourceRGB 1 0 0
  fillPreserve
  setSourceRGB 0 0 0
  stroke
  moveTo ux (h + padding/2)
  lineTo (ux + uw) (h + padding/2)
  stroke
  moveTo (-w/2) (h/2 + padding)
  showText text
  moveTo (-w1/2) (h + h1/2 + 2 * padding)
  showText text1

roundedRect x y w h = do
  moveTo       x            (y+pad)
  lineTo       x            (y + h - pad)
  arcNegative (x + pad)     (y + h - pad) pad pi (pi/2)
  lineTo      (x + w - pad) (y + h)
  arcNegative (x + w - pad) (y + h - pad) pad (pi/2) 0
  lineTo      (x + w)       (y + pad)
  arcNegative (x + w - pad) (y + pad)     pad 0  (-pi/2)
  lineTo      (x + pad)      y
  arcNegative (x + pad)     (y + pad)     pad (-pi/2) (-pi)
  closePath

  where pad = 1/10 * min w h
