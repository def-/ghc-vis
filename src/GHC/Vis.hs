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
  vis,
  fullVis,
  view,
  eval,
  switch,
  update,
  clear,
  export
  )
  where

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch, error)
#else
import Prelude hiding (error)
#endif

import Graphics.UI.Gtk hiding (Box, Signal, get)
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
import Graphics.Rendering.Cairo.SVG

import Paths_ghc_vis as My

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
positionIncrement = 50

bigPositionIncrement :: Double
bigPositionIncrement = 200

signalTimeout :: Int
signalTimeout = 1000000

-- | This is the main function. It's to be called from GHCi and launches a
--   graphical window in a new thread.
vis :: IO ()
vis = do
  vr <- swapMVar visRunning True
  unless vr $ void $ forkIO visMainThread

fullVis :: IO ()
fullVis = do
  vr <- swapMVar visRunning True
  unless vr $ void $ forkIO fullVisMainThread

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
export filename = export' filename >> return ()

export' :: String -> IO (Maybe String)
export' filename = case mbDrawFn of
  Right error -> do putStrLn error
                    return $ Just error
  Left _ -> do put $ ExportSignal ((\(Left x) -> x) mbDrawFn) filename
               return Nothing

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
    when (E.eventButton e == LeftButton && E.eventClick e == SingleClick) $
      join $ runCorrect click

    when (E.eventButton e == RightButton && E.eventClick e == SingleClick) $
      modifyIORef visState (\s -> s {dragging = True})

    when (E.eventButton e == MiddleButton && E.eventClick e == SingleClick) $ do
      modifyIORef visState (\s -> s {zoomRatio = 1, position = (0, 0)})
      widgetQueueDraw canvas

    return True

  onButtonRelease canvas $ \e -> do
    when (E.eventButton e == RightButton) $
      modifyIORef visState (\s -> s {dragging = False})

    return True

  onScroll canvas $ \e -> do
    state <- readIORef visState

    when (E.eventDirection e == ScrollUp) $ do
      let newZoomRatio = zoomRatio state * zoomIncrement
      newPos <- zoomImage canvas state newZoomRatio (mousePos state)
      modifyIORef visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

    when (E.eventDirection e == ScrollDown) $ do
      let newZoomRatio = zoomRatio state / zoomIncrement
      newPos <- zoomImage canvas state newZoomRatio (mousePos state)
      modifyIORef visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

    widgetQueueDraw canvas
    return True

  onKeyPress window $ \e -> do
    --putStrLn $ E.eventKeyName e

    state <- readIORef visState

    when (E.eventKeyName e `elem` ["plus", "Page_Up", "KP_Add"]) $ do
      let newZoomRatio = zoomRatio state * zoomIncrement
          (oldX, oldY) = position state
          newPos = (oldX*zoomIncrement, oldY*zoomIncrement)
      modifyIORef visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

    when (E.eventKeyName e `elem` ["minus", "Page_Down", "KP_Subtract"]) $ do
      let newZoomRatio = zoomRatio state / zoomIncrement
          (oldX, oldY) = position state
          newPos = (oldX/zoomIncrement, oldY/zoomIncrement)
      modifyIORef visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

    when (E.eventKeyName e `elem` ["0", "equal"]) $
      modifyIORef visState (\s -> s {zoomRatio = 1, position = (0, 0)})

    when (E.eventKeyName e `elem` ["Left", "h", "a"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newX  = x + positionIncrement
        in s {position = (newX, y)})

    when (E.eventKeyName e `elem` ["Right", "l", "d"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newX  = x - positionIncrement
        in s {position = (newX, y)})

    when (E.eventKeyName e `elem` ["Up", "k", "w"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newY  = y + positionIncrement
        in s {position = (x, newY)})

    when (E.eventKeyName e `elem` ["Down", "j", "s"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newY  = y - positionIncrement
        in s {position = (x, newY)})

    when (E.eventKeyName e `elem` ["H", "A"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newX  = x + bigPositionIncrement
        in s {position = (newX, y)})

    when (E.eventKeyName e `elem` ["L", "D"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newX  = x - bigPositionIncrement
        in s {position = (newX, y)})

    when (E.eventKeyName e `elem` ["K", "W"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newY  = y + bigPositionIncrement
        in s {position = (x, newY)})

    when (E.eventKeyName e `elem` ["J", "S"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newY  = y - bigPositionIncrement
        in s {position = (x, newY)})

    when (E.eventKeyName e `elem` ["space", "Return", "KP_Enter"]) $
      join $ runCorrect click

    when (E.eventKeyName e `elem` ["v"]) $
      put SwitchSignal

    when (E.eventKeyName e `elem` ["c"]) $
      put ClearSignal

    when (E.eventKeyName e `elem` ["u"]) $
      put UpdateSignal

    widgetQueueDraw canvas
    return True

  widgetShowAll window

  dummy <- windowNew
  reactThread <- forkIO $ react canvas dummy window
  --onDestroy window mainQuit -- Causes :r problems with multiple windows
  onDestroy window (quit reactThread)

  mainGUI
  return ()

myFileSave :: FileChooserDialog -> ResponseId -> IO ()
myFileSave fcdialog response = do
  case response of
    ResponseOk -> do Just filename <- fileChooserGetFilename fcdialog
                     mbError <- export' filename
                     case mbError of
                       Nothing -> return ()
                       Just error -> do
                         errorDialog <- messageDialogNew Nothing [] MessageError ButtonsOk error
                         widgetShow errorDialog
                         onResponse errorDialog $ const $ widgetHide errorDialog
                         return ()
    _ -> return ()
  widgetHide fcdialog

myNewFilter filter name dialog = do
  filt <- fileFilterNew
  fileFilterAddPattern filt filter
  fileFilterSetName filt $ name ++ " (" ++ filter ++ ")"
  fileChooserAddFilter dialog filt

fullVisMainThread :: IO ()
fullVisMainThread = do
  initGUI

  mainUIFile <- My.getDataFileName "data/main.ui"
  builder <- builderNew
  builderAddFromFile builder mainUIFile

  let get :: forall cls . GObjectClass cls
          => (GObject -> cls)
          -> String
          -> IO cls
      get = builderGetObject builder

  window       <- get castToWindow "window"
  canvas       <- get castToDrawingArea "drawingarea"

  saveDialog   <- get castToFileChooserDialog "savedialog"
  aboutDialog  <- get castToAboutDialog "aboutdialog"

  legendDialog <- get castToWindow "legenddialog"
  legendCanvas <- get castToDrawingArea "legenddrawingarea"

  myNewFilter "*.pdf" "PDF" saveDialog
  myNewFilter "*.svg" "SVG" saveDialog
  myNewFilter "*.ps" "PostScript" saveDialog
  myNewFilter "*.png" "PNG" saveDialog

  onResponse saveDialog $ myFileSave saveDialog
  onResponse aboutDialog $ const $ widgetHide aboutDialog

  onDelete saveDialog   $ const $ widgetHide saveDialog   >> return True
  onDelete aboutDialog  $ const $ widgetHide aboutDialog  >> return True
  onDelete legendDialog $ const $ widgetHide legendDialog >> return True

  get castToMenuItem "clear"  >>= \item -> onActivateLeaf item clear
  get castToMenuItem "switch" >>= \item -> onActivateLeaf item switch
  get castToMenuItem "update" >>= \item -> onActivateLeaf item update
  get castToMenuItem "export" >>= \item -> onActivateLeaf item $ widgetShow saveDialog
  get castToMenuItem "quit"   >>= \item -> onActivateLeaf item $ widgetDestroy window
  get castToMenuItem "about"  >>= \item -> onActivateLeaf item $ widgetShow aboutDialog
  get castToMenuItem "legend" >>= \item -> onActivateLeaf item $ widgetShow legendDialog

  widgetModifyBg canvas StateNormal backgroundColor
  widgetModifyBg legendCanvas StateNormal backgroundColor

  legendListSVG  <- My.getDataFileName "data/legend_list.svg" >>= svgNewFromFile
  legendGraphSVG <- My.getDataFileName "data/legend_graph.svg" >>= svgNewFromFile

  onExpose canvas $ const $ do
    runCorrect redraw >>= \f -> f canvas
    runCorrect move >>= \f -> f canvas
    return True

  onExpose legendCanvas $ const $ do
    state <- readIORef visState
    E.Rectangle _ _ rw2 rh2 <- widgetGetAllocation legendCanvas
    win <- widgetGetDrawWindow legendCanvas
    renderWithDrawable win $ do
      let svg = case T.view state of
            ListView  -> legendListSVG
            GraphView -> legendGraphSVG
          (cx, cy) = svgGetSize svg

          rw = 0.9 * fromIntegral rw2
          rh = 0.9 * fromIntegral rh2

          -- Proportional scaling
          (sx,sy) = (min (rw / fromIntegral cx) (rh / fromIntegral cy), sx)
      scale sx sy
      svgRender svg

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
    when (E.eventButton e == LeftButton && E.eventClick e == SingleClick) $
      join $ runCorrect click

    when (E.eventButton e == RightButton && E.eventClick e == SingleClick) $
      modifyIORef visState (\s -> s {dragging = True})

    when (E.eventButton e == MiddleButton && E.eventClick e == SingleClick) $ do
      modifyIORef visState (\s -> s {zoomRatio = 1, position = (0, 0)})
      widgetQueueDraw canvas

    return True

  onButtonRelease canvas $ \e -> do
    when (E.eventButton e == RightButton) $
      modifyIORef visState (\s -> s {dragging = False})

    return True

  onScroll canvas $ \e -> do
    state <- readIORef visState

    when (E.eventDirection e == ScrollUp) $ do
      let newZoomRatio = zoomRatio state * zoomIncrement
      newPos <- zoomImage canvas state newZoomRatio (mousePos state)
      modifyIORef visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

    when (E.eventDirection e == ScrollDown) $ do
      let newZoomRatio = zoomRatio state / zoomIncrement
      newPos <- zoomImage canvas state newZoomRatio (mousePos state)
      modifyIORef visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

    widgetQueueDraw canvas
    return True

  onKeyPress window $ \e -> do
    --putStrLn $ E.eventKeyName e

    state <- readIORef visState

    when (E.eventKeyName e `elem` ["plus", "Page_Up", "KP_Add"]) $ do
      let newZoomRatio = zoomRatio state * zoomIncrement
          (oldX, oldY) = position state
          newPos = (oldX*zoomIncrement, oldY*zoomIncrement)
      modifyIORef visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

    when (E.eventKeyName e `elem` ["minus", "Page_Down", "KP_Subtract"]) $ do
      let newZoomRatio = zoomRatio state / zoomIncrement
          (oldX, oldY) = position state
          newPos = (oldX/zoomIncrement, oldY/zoomIncrement)
      modifyIORef visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

    when (E.eventKeyName e `elem` ["0", "equal"]) $
      modifyIORef visState (\s -> s {zoomRatio = 1, position = (0, 0)})

    when (E.eventKeyName e `elem` ["Left", "h", "a"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newX  = x + positionIncrement
        in s {position = (newX, y)})

    when (E.eventKeyName e `elem` ["Right", "l", "d"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newX  = x - positionIncrement
        in s {position = (newX, y)})

    when (E.eventKeyName e `elem` ["Up", "k", "w"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newY  = y + positionIncrement
        in s {position = (x, newY)})

    when (E.eventKeyName e `elem` ["Down", "j", "s"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newY  = y - positionIncrement
        in s {position = (x, newY)})

    when (E.eventKeyName e `elem` ["H", "A"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newX  = x + bigPositionIncrement
        in s {position = (newX, y)})

    when (E.eventKeyName e `elem` ["L", "D"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newX  = x - bigPositionIncrement
        in s {position = (newX, y)})

    when (E.eventKeyName e `elem` ["K", "W"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newY  = y + bigPositionIncrement
        in s {position = (x, newY)})

    when (E.eventKeyName e `elem` ["J", "S"]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newY  = y - bigPositionIncrement
        in s {position = (x, newY)})

    when (E.eventKeyName e `elem` ["space", "Return", "KP_Enter"]) $
      join $ runCorrect click

    when (E.eventKeyName e `elem` ["v"]) $
      put SwitchSignal

    when (E.eventKeyName e `elem` ["c"]) $
      put ClearSignal

    when (E.eventKeyName e `elem` ["u"]) $
      put UpdateSignal

    widgetQueueDraw canvas
    return True

  widgetShowAll window

  reactThread <- forkIO $ react canvas legendCanvas window
  --onDestroy window mainQuit -- Causes :r problems with multiple windows
  onDestroy window (quit reactThread)

  mainGUI
  return ()

quit :: ThreadId -> IO ()
quit reactThread = do
  swapMVar visRunning False
  killThread reactThread

react :: (WidgetClass w1, WidgetClass w2, WidgetClass w3) => w1 -> w2 -> w3 -> IO b
react canvas legendCanvas window = do
  -- Timeout used to handle ghci reloads (:r)
  -- Reloads cause the visSignal to be reinitialized, but takeMVar is still
  -- waiting for the old one.  This solution is not perfect, but it works for
  -- now.
  mbSignal <- timeout signalTimeout (takeMVar visSignal)
  case mbSignal of
    Nothing -> do
      running <- readMVar visRunning
      if running then react canvas legendCanvas window else
        -- :r caused visRunning to be reset
        (do swapMVar visRunning True
            timeout signalTimeout (putMVar visSignal UpdateSignal)
            react canvas legendCanvas window)
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
      postGUISync $ widgetQueueDraw legendCanvas
      react canvas legendCanvas window

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

zoomImage :: WidgetClass w1 => w1 -> State -> Double -> T.Point -> IO T.Point
zoomImage _canvas s newZoomRatio _mousePos@(_x', _y') = do
  -- TODO: Mouse must stay at same spot

  --E.Rectangle _ _ rw' rh' <- widgetGetAllocation canvas
  --let (rw, rh) = (fromIntegral rw', fromIntegral rh')
  --let (x,y) = (x' - rw / 2, y' - rh / 2)

  let (oldPosX, oldPosY) = position s
      zoom = newZoomRatio / zoomRatio s
      newPos = (oldPosX * zoom, oldPosY * zoom)

  return newPos

-- Zoom into mouse, but only working from (0,0)
-- newPos = ( oldPosX + x * zoomRatio s - x * newZoomRatio
--          , oldPosY + y * zoomRatio s - y * newZoomRatio )

-- Zoom into center:
-- newPos = ( oldPosX * zoomIncrement
--          , oldPosY * zoomIncrement )
