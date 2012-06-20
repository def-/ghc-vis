module GHC.Vis.GTK (
  tvis,
  vis
  )
  where
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Control.Concurrent
import Control.Concurrent.MVar

import GHC.Vis
import GHC.HeapView

tvis ref bref = do
  forkIO $ vis ref bref

vis ref bref = do
  initGUI
  window <- windowNew
  onDestroy window mainQuit

  canvas <- drawingAreaNew

  set window [ windowTitle := "Vis"
             , containerChild := canvas
             ]

  onExpose canvas $ const $ do
    redraw canvas ref
    return True

  widgetShowAll window

  forkIO $ react bref window

  mainGUI

react bref window = do
  takeMVar bref
  widgetQueueDraw window
  react bref window

redraw canvas ref = do
  boxes <- readMVar ref
  texts <- mapM (\(Box a) -> bprint a) boxes
  render canvas $ do
    setSourceRGB 0 0 0
    moveTo 10 10
    showText $ show texts

render canvas r = do
        win <- widgetGetDrawWindow canvas
        renderWithDrawable win $ do
          r
