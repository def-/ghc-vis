{-# LANGUAGE CPP, RankNTypes, ImpredicativeTypes #-}
{- |
   Module      : GHC.Vis
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

Although ghc-vis is meant to be used in GHCi it can also be used as a library
in regular Haskell programs which are run or compiled by GHC. You can run those
programs using \"runghc example.hs\" or \"ghc -threaded example.hs && ./example\".
Without the \"-threaded\"-Flag ghc-vis does not work correctly. This is an
example using ghc-vis outside of GHCi:

> import GHC.Vis
>
> main = do
>   putStrLn "Start"
>   let a = "teeest"
>   let b = [1..3]
>   let c = b ++ b
>   let d = [1..]
>   putStrLn $ show $ d !! 1
>
>   visualization
>   view a "a"
>   view b "b"
>   view c "c"
>   view d "d"
>
>   getChar
>   switch
>
>   getChar
>   putStrLn "End"
 -}
module GHC.Vis (
  visualization, -- TODO: Maybe rename to vis
  view,
  eval,
  switch,
  update,
  clear,
  export
  )
  where

import Prelude hiding (catch, error)

import Graphics.UI.Gtk hiding (Box, Signal)
import qualified Graphics.UI.Gtk.Gdk.Events as E

import System.IO
import Control.Concurrent
import Control.Monad

import Control.Exception hiding (evaluate)

import Data.Char
import Data.IORef

import System.Timeout
import System.Mem

import GHC.HeapView hiding (name)

import GHC.Vis.Types hiding (view)
import qualified GHC.Vis.Types as T
import GHC.Vis.View.Common
import qualified GHC.Vis.View.List as List

#ifdef GRAPH_VIEW
import Data.GraphViz.Commands
import qualified GHC.Vis.View.Graph as Graph
#endif

import Graphics.Rendering.Cairo

views :: [View]
views =
  View List.redraw List.click List.move List.updateObjects List.export :
#ifdef GRAPH_VIEW
  View Graph.redraw Graph.click Graph.move Graph.updateObjects Graph.export :
#endif
  []

title :: String
title = "ghc-vis"

backgroundColor :: Color
backgroundColor = Color 0xffff 0xffff 0xffff

defaultSize :: (Int, Int)
defaultSize = (640, 480)

zoomIncrement :: Double
zoomIncrement = 1.25

positionIncrement :: Double
positionIncrement = 100

signalTimeout :: Int
signalTimeout = 1000000

-- | This is the main function. It's to be called from GHCi and launches a
--   graphical window in a new thread.
visualization :: IO ()
visualization = do
  vr <- swapMVar visRunning True
  unless vr $ void $ forkIO visMainThread

-- | Add expressions with a name to the visualization window.
view :: a -> String -> IO ()
view a name = put $ NewSignal (asBox a) name

-- | Evaluate an object that is shown in the visualization. (Names start with 't')
eval :: String -> IO ()
eval t = evaluate t >> update

-- | Switch between the list view and the graph view
switch :: IO ()
switch = put SwitchSignal

-- | When an object is updated by accessing it, you have to call this to
--   refresh the visualization window. You can also click on an object to force
--   an update.
update :: IO ()
update = put UpdateSignal

-- | Clear the visualization window, removing all expressions from it.
clear :: IO ()
clear = put ClearSignal

-- | Export the current visualization view to a file, format depends on the
--   file ending. Currently supported: svg, png, pdf, ps
export :: String -> IO ()
export filename = case mbDrawFn of
  Right error -> putStrLn error
  Left _ -> put $ ExportSignal ((\(Left x) -> x) mbDrawFn) filename

  where mbDrawFn = case map toLower (reverse . take 4 . reverse $ filename) of
          ".svg"  -> Left withSVGSurface
          ".pdf"  -> Left withPDFSurface
          ".png"  -> Left withPNGSurface
          _:".ps" -> Left withPSSurface
          _       -> Right "Unknown file extension, try one of the following: .svg, .pdf, .ps, .png"

        withPNGSurface filePath width height action =
          withImageSurface FormatARGB32 (ceiling width) (ceiling height) $
          \surface -> do
            ret <- action surface
            surfaceWriteToPNG surface filePath
            return ret

put :: Signal -> IO ()
put s = void $ timeout signalTimeout $ putMVar visSignal s

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
    runCorrect redraw >>= \f -> f canvas
    runCorrect move >>= \f -> f canvas
    return True

  onMotionNotify canvas False $ \e -> do
    state <- readIORef visState
    modifyIORef visState (\s -> s {mousePos = (E.eventX e, E.eventY e)})

    if dragging state
    then do
      let (oldX, oldY) = mousePos state
          (deltaX, deltaY) = (E.eventX e - oldX, E.eventY e - oldY)
          (oldPosX, oldPosY) = position state
      modifyIORef visState (\s -> s {position = (oldPosX + deltaX, oldPosY + deltaY)})
      widgetQueueDraw canvas
    else
      runCorrect move >>= \f -> f canvas

    return True

  onButtonPress canvas $ \e -> do
    when (E.eventButton e == LeftButton && E.eventClick e == SingleClick) $ do
      modifyIORef visState (\s -> s {dragging = True})

    when (E.eventButton e == MiddleButton && E.eventClick e == SingleClick) $ do
      modifyIORef visState (\s -> s {zoomRatio = 1, position = (0, 0)})
      widgetQueueDraw canvas

    return True

  onButtonRelease canvas $ \e -> do
    putStrLn $ show $ E.eventButton e

    cf <- runCorrect click
    when (E.eventButton e == LeftButton) $ do
      modifyIORef visState (\s -> s {dragging = False})
      cf

    return True

  onScroll canvas $ \e -> do
    state <- readIORef visState

    -- rect = self.get_allocation()
    -- x, y = pos
    -- x -= 0.5*rect.width
    -- y -= 0.5*rect.height
    -- self.x += x / self.zoom_ratio - x / zoom_ratio
    -- self.y += y / self.zoom_ratio - y / zoom_ratio

    -- TODO: Mouse must stay at same spot
    E.Rectangle _ _ rw rh <- widgetGetAllocation canvas

    let (x, y) = mousePos state
        (oldPosX, oldPosY) = position state

    when (E.eventDirection e == ScrollUp) $
      modifyIORef visState (\s ->
        let newZoomRatio = zoomRatio s * zoomIncrement
            (newX, newY) = (oldPosX + x * zoomRatio s - x * newZoomRatio, oldPosY + y * zoomRatio s - y * newZoomRatio)
        in s {zoomRatio = newZoomRatio, position = (newX, newY)})

    when (E.eventDirection e == ScrollDown) $
      modifyIORef visState (\s ->
        let newZoomRatio = zoomRatio s / zoomIncrement
            (newX, newY) = (oldPosX + x * zoomRatio s - x * newZoomRatio, oldPosY + y * zoomRatio s - y * newZoomRatio)
        in s {zoomRatio = newZoomRatio, position = (newX, newY)})

    widgetQueueDraw canvas
    return True

  onKeyPress window $ \e -> do
    putStrLn $ E.eventKeyName e

    when (E.eventKeyName e `elem` ["plus", "Page_Up", "Add"]) $
      modifyIORef visState (\s -> s {zoomRatio = zoomRatio s * zoomIncrement})

    when (E.eventKeyName e `elem` ["minus", "Page_Down", "Subtract"]) $
      modifyIORef visState (\s -> s {zoomRatio = zoomRatio s / zoomIncrement})

    when (E.eventKeyName e `elem` ["0", "Equal"]) $
      modifyIORef visState (\s -> s {zoomRatio = 1, position = (0, 0)})

    when (E.eventKeyName e `elem` ["Left", "h", "a"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newX  = x + positionIncrement * zoomRatio s
        in s {position = (newX, y)})

    when (E.eventKeyName e `elem` ["Right", "l", "d"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newX  = x - positionIncrement * zoomRatio s
        in s {position = (newX, y)})

    when (E.eventKeyName e `elem` ["Up", "k", "w"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newY  = y + positionIncrement * zoomRatio s
        in s {position = (x, newY)})

    when (E.eventKeyName e `elem` ["Down", "j", "s"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newY  = y - positionIncrement * zoomRatio s
        in s {position = (x, newY)})

    widgetQueueDraw canvas
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
            timeout signalTimeout (putMVar visSignal UpdateSignal)
            react canvas window)
    Just signal -> do
      case signal of
        NewSignal x n  -> modifyMVar_ visBoxes (
          \y -> if (x,n) `elem` y then return y else return $ y ++ [(x,n)])
        ClearSignal    -> modifyMVar_ visBoxes (\_ -> return [])
        UpdateSignal   -> return ()
        SwitchSignal   -> doSwitch
        ExportSignal d f -> catch (runCorrect exportView >>= \e -> e d f)
          (\e -> do let err = show (e :: IOException)
                    hPutStrLn stderr $ "Couldn't export to file \"" ++ f ++ "\": " ++ err
                    return ())

      boxes <- readMVar visBoxes
      performGC -- TODO: Else Blackholes appear. Do we want this?
                -- Blackholes stop our current thread and only resume after
                -- they have been replaced with their result, thereby leading
                -- to an additional element in the HeapMap we don't want.
                -- Example for bad behaviour that would happen then:
                -- λ> let xs = [1..42] :: [Int]
                -- λ> let x = 17 :: Int
                -- λ> let ys = [ y | y <- xs, y >= x ]

      runCorrect updateObjects >>= \f -> f boxes

      postGUISync $ widgetQueueDraw canvas
      react canvas window

#ifdef GRAPH_VIEW
  where doSwitch = isGraphvizInstalled >>= \gvi -> if gvi
          then modifyIORef visState (\s -> s {T.view = succN (T.view s), zoomRatio = 1, position = (0, 0)})
          else putStrLn "Cannot switch view: Graphviz not installed"

        succN GraphView = ListView
        succN ListView = GraphView
#else
  where doSwitch = putStrLn "Cannot switch view: Graph view disabled at build"
#endif

runCorrect :: (View -> f) -> IO f
runCorrect f = do
  s <- readIORef visState
  return $ f $ views !! fromEnum (T.view s)
