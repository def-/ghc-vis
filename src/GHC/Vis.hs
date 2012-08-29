{- |
   Module      : GHC.Vis
   Description : Live visualization of data structures in GHC
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

   Live visualisation of data structures in GHC

   To use this package add the accompanying @ghci@ file to your @.ghci@ like this:

   > :script $PATH_TO_GHC-VIS/ghci

   Now you can run ghci and experiment with @ghc-vis@. Start the visualization:

   > $ ghci
   > GHCi, version 7.4.2: http://www.haskell.org/ghc/  :? for help
   > Ok, modules loaded: none.
   > > :vis

   A blank window should appear now. This is the visualization window. Add an
   expression to the visualization:

   > > let a = [1..5]
   > > :view a
   > > :view "foo"

   Evaluate an object that is shown in the visualization. You can also click on
   the object to evaluate it.

   > > :eval t0

   Switch between the list view and the graph view:

   > > :switch

   When an object is updated by using its values, you have to call @:update@ to
   refresh the visualization window. You can also click on an object to force an
   update:

   > > a !! 2
   > 3
   > > :update

   Clear the visualization window, this also happens when you @:load@ or
   @:reload@ a source file:

   > > :clear
 -}
module GHC.Vis (
  visualization,
  visSignal,
  evaluate
  )
  where

import Prelude hiding (catch)

import Graphics.UI.Gtk hiding (Box, Signal)
import qualified Graphics.UI.Gtk.Gdk.Events as E

import System.IO
import Control.Concurrent
import Control.Monad

import Control.Exception hiding (evaluate)

import Data.IORef

import System.Timeout
import System.Mem

import GHC.Vis.Types
import GHC.Vis.GTK.Common
import qualified GHC.Vis.GTK.Graph as Graph
import qualified GHC.Vis.GTK.List as List

title :: String
title = "ghc-vis"

backgroundColor :: Color
backgroundColor = Color 0xffff 0xffff 0xffff

defaultSize :: (Int, Int)
defaultSize = (640, 480)

signalTimeout :: Int
signalTimeout = 1000000

-- | This is the main function. It's to be called from GHCi and launches a
--   graphical window in a new thread.
visualization :: IO ()
visualization = do
  vr <- swapMVar visRunning True
  unless vr $ void $ forkIO visMainThread

visMainThread :: IO ()
visMainThread = do
  initGUI
  window <- windowNew

  canvas <- drawingAreaNew

  widgetModifyBg canvas StateNormal backgroundColor

  set window [ windowTitle := title
             , containerChild := canvas
             ]
  (uncurry $ windowSetDefaultSize window) defaultSize

  onExpose canvas $ const $ do
    runCorrect Graph.redraw List.redraw >>= \f -> f canvas
    runCorrect Graph.move List.move >>= \f -> f canvas
    return True

  onMotionNotify canvas False $ \e -> do
    modifyIORef visState (\s -> s {mousePos = (E.eventX e, E.eventY e)})
    runCorrect Graph.move List.move >>= \f -> f canvas
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
  mbSignal <- timeout signalTimeout (takeMVar visSignal)
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
        NewSignal x n  -> modifyMVar_ visBoxes (
          \y -> if (x,n) `elem` y then return y else return $ y ++ [(x,n)])
        ClearSignal    -> modifyMVar_ visBoxes (\_ -> return [])
        UpdateSignal   -> return ()
        SwitchSignal   -> modifyIORef visState (\s -> s {view = succN (view s)})
        ExportSignal f -> catch (runCorrect Graph.export List.export >>= \e -> e f)
          (\e -> do let err = show (e :: IOException)
                    hPutStrLn stderr $ "Couldn't export to file \"" ++ f ++ "\": " ++ err
                    return ())

      boxes <- readMVar visBoxes
      performGC -- TODO: Else Blackholes appear. Do we want this?

      runCorrect Graph.updateObjects List.updateObjects >>= \f -> f boxes

      postGUISync $ widgetQueueDraw canvas
      react canvas window

  where succN v = if v == maxBound then minBound else succ v

runCorrect :: f -> f -> IO f
runCorrect f1 f2 = do
  s <- readIORef visState
  return $ case view s of
             GraphView -> f1
             ListView  -> f2
