module GHC.Vis.GTK (
  tvis,
  vis
  )
  where
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Control.Concurrent

import Data.IORef

import GHC.Vis
import GHC.HeapView

tvis ref = do
  forkIO $ vis ref

vis ref = do
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
  mainGUI

redraw canvas ref = do
  boxes <- readIORef ref
  texts <- mapM (\(Box a) -> bprint a) boxes
  render canvas ref $ do
    setSourceRGB 0 0 0
    moveTo 10 10
    showText $ show texts

render canvas ref r = do
        win <- widgetGetDrawWindow canvas
        renderWithDrawable win $ do
          r
