module GHC.Vis (
  visualization,
  visSignal,
  evaluate
  )
  where

import Graphics.UI.Gtk hiding (Box, Signal)
import qualified Graphics.UI.Gtk.Gdk.Events as E

import Control.Concurrent
import Control.Monad

import Data.IORef

import System.Timeout
import System.Mem

import GHC.Vis.Internal hiding (boxes)
import GHC.Vis.Types hiding (boxes)
import GHC.Vis.GTK.Common
import qualified GHC.Vis.GTK.Graph as Graph
import qualified GHC.Vis.GTK.List as List

visualization :: IO ()
visualization = do
  vr <- swapMVar visRunning True
  unless vr $ void $ forkIO visMainThread

visMainThread :: IO ()
visMainThread = do
  initGUI
  window <- windowNew

  canvas <- drawingAreaNew

  set window [ windowTitle := "Vis"
             , containerChild := canvas
             ]

  onExpose canvas $ const $ do
    runCorrect Graph.redraw List.redraw >>= \f -> f canvas
    runCorrect Graph.tick List.tick >>= \f -> f canvas
    return True

  onMotionNotify canvas False $ \e -> do
    modifyIORef visState (\s -> s {mousePos = (E.eventX e, E.eventY e)})
    runCorrect Graph.tick List.tick >>= \f -> f canvas
    return True

  onButtonPress canvas $ \e -> do
    click <- runCorrect Graph.click List.click
    when (E.eventButton e == LeftButton && E.eventClick e == SingleClick) click
    return True

  widgetShowAll window

  reactThread <- forkIO $ react canvas window
  --onDestroy window mainQuit -- Causes :r problems with multiple windows
  onDestroy window (quit reactThread)

  mainGUI
  return ()

quit :: ThreadId -> IO ()
quit reactThread = do
  swapMVar visRunning False
  killThread reactThread

react :: (WidgetClass w1, WidgetClass w2) => w1 -> w2 -> IO b
react canvas window = do
  -- Timeout used to handle ghci reloads (:r)
  -- Reloads cause the visSignal to be reinitialized, but takeMVar is still
  -- waiting for the old one.  This solution is not perfect, but it works for
  -- now.
  mbSignal <- timeout 1000000 (takeMVar visSignal)
  case mbSignal of
    Nothing -> do
      running <- readMVar visRunning
      if running then react canvas window else
        -- :r caused visRunning to be reset
        (do swapMVar visRunning True
            putMVar visSignal UpdateSignal
            react canvas window)
    Just signal -> do
      case signal of
        NewSignal x n -> modifyMVar_ visBoxes (
          \y -> if (x,n) `elem` y then return y else return $ y ++ [(x,n)])
        ClearSignal   -> modifyMVar_ visBoxes (\_ -> return [])
        UpdateSignal  -> return ()
        SwitchSignal  -> modifyIORef visState (\s -> s {mode = not (mode s)})

      boxes <- readMVar visBoxes
      performGC -- TODO: Else Blackholes appear. Do we want this?

      runCorrect Graph.updateObjects List.updateObjects >>= \f -> f boxes

      postGUISync $ widgetQueueDraw canvas
      react canvas window

runCorrect :: f -> f -> IO f
runCorrect f1 f2 = do
  s <- readIORef visState
  return $ if mode s then f1 else f2
