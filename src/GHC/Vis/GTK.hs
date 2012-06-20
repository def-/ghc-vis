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

import GHC.Vis
import GHC.HeapView

import System.IO.Unsafe

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
    UpdateSignal -> return ()

  threadDelay 10000 -- 10 ms, else sometimes redraw happens too fast (Why?)
  widgetQueueDraw canvas
  react ref bref canvas

redraw canvas bref = do
  boxes <- readMVar bref
  texts <- mapM (\(Box a) -> bprint a) boxes
  render canvas $ do
    setSourceRGB 0 0 0
    moveTo 10 10
    showText $ show texts

render canvas r = do
        win <- widgetGetDrawWindow canvas
        renderWithDrawable win $ do
          r
