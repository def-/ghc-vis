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
  move,
  updateObjects
  )
  where

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif

import Graphics.UI.Gtk hiding (Box, Signal, Rectangle, draw)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo hiding (x, y)

import Control.Concurrent
import Control.Monad
import Control.Exception

import Data.Maybe
import Data.IORef
import System.IO.Unsafe

import GHC.Vis.View.Graph.Parser
import GHC.Vis.Types hiding (State, View(..))
import GHC.Vis.View.Common

import GHC.HeapView hiding (size)

import Graphics.XDot.Viewer
import Graphics.XDot.Types hiding (size, w, h)

import Graphics.Rendering.Cairo.SVG
import Paths_ghc_vis as My

hoverIconWidth :: Double
hoverIconWidth = 35

hoverIconHeight :: Double
hoverIconHeight = 17.5

hoverIconSpace :: Double
hoverIconSpace = 5

data Icon = EvaluateIcon
          | CollapseIcon
          deriving (Show, Eq)

data State = State
  { boxes      :: [Box]
  , operations :: [(Object Int, Operation)]
  , totalSize  :: Rectangle
  , bounds     :: [(Object Int, Rectangle)]
  , hoverIconBounds :: [(Object Int, [(Icon, Rectangle)])]
  , hover      :: Object Int
  , iconHover  :: Maybe (Object Int, Icon)
  }

state :: IORef State
{-# NOINLINE state #-}
state = unsafePerformIO $ newIORef $ State [] [] (0, 0, 1, 1) [] [] None Nothing

iconEvaluateSVG :: SVG
{-# NOINLINE iconEvaluateSVG #-}
iconEvaluateSVG = unsafePerformIO $ My.getDataFileName "data/icon_evaluate.svg" >>= svgNewFromFile
iconCollapseSVG :: SVG
{-# NOINLINE iconCollapseSVG #-}
iconCollapseSVG = unsafePerformIO $ My.getDataFileName "data/icon_collapse.svg" >>= svgNewFromFile
hoverEvaluateSVG :: SVG
{-# NOINLINE hoverEvaluateSVG #-}
hoverEvaluateSVG = unsafePerformIO $ My.getDataFileName "data/hover_evaluate.svg" >>= svgNewFromFile
hoverCollapseSVG :: SVG
{-# NOINLINE hoverCollapseSVG #-}
hoverCollapseSVG = unsafePerformIO $ My.getDataFileName "data/hover_collapse.svg" >>= svgNewFromFile

-- | Draw visualization to screen, called on every update or when it's
--   requested from outside the program.
redraw :: WidgetClass w => w -> Render ()
redraw canvas = do
  s <- liftIO $ readIORef state
  rw2 <- liftIO $ Gtk.widgetGetAllocatedWidth canvas
  rh2 <- liftIO $ Gtk.widgetGetAllocatedHeight canvas

  (bbs, hibbs) <- draw s rw2 rh2

  liftIO $ modifyIORef state (\s' -> s' {bounds = bbs, hoverIconBounds = hibbs})

-- | Export the visualization to an SVG file
export :: DrawFunction -> String -> IO ()
export drawFn file = do
  s <- readIORef state

  let (_, _, xSize, ySize) = totalSize s

  drawFn file xSize ySize
    (\surface -> renderWith surface (draw s (round xSize) (round ySize)))

  return ()

draw :: State -> Int -> Int -> Render ([(Object Int, Rectangle)], [(Object Int, [(Icon, Rectangle)])])
draw s rw2 rh2 =
  if null $ boxes s then return ([], [])
  else do
    vS <- liftIO $ readIORef visState

    -- Line widths don't count to size, let's add a bit
    let rw = 0.97 * fromIntegral rw2
        rh = 0.97 * fromIntegral rh2

        ops = operations s
        size@(_,_,sw,sh) = totalSize s

    -- Proportional scaling
        (sx,sy) = (min 1000 $ zoomRatio vS * minimum [2, rw / sw, rh / sh], sx)
        (ox1,oy1) = (0.5 * fromIntegral rw2, 0.5 * fromIntegral rh2)
        (ox2,oy2) = position vS
        (ox,oy) = (ox1 + ox2, oy1 + oy2)

    translate ox oy
    scale sx sy

    result <- drawAll (hover s) size ops

    case hover s of
      Node n -> do
        let Just (x,y,w,_h) = lookup (Node n) result

        translate (x + w + hoverIconSpace) y
        drawHoverMenu $ iconHover s
      _      -> return True

    let trafo (o, (x,y,w,h)) = (o,
          ( x * sx + ox -- Transformations to correct scaling and offset
          , y * sy + oy
          , w * sx
          , h * sy
          ))

    let toHoverIconBounds (o, (x,y,w,_h)) = (o, map trafo
          [ (EvaluateIcon, (x+w, y, hoverIconWidth + hoverIconSpace, hoverIconHeight))
          , (CollapseIcon, (x+w, y+hoverIconHeight, hoverIconWidth + hoverIconSpace, hoverIconHeight))
          ])

    return (map trafo result, map toHoverIconBounds result)

drawHoverMenu :: Maybe (t, Icon) -> Render Bool
drawHoverMenu x = do
  svgRender $ case x of
    Just (_, EvaluateIcon) -> hoverEvaluateSVG
    _                      -> iconEvaluateSVG

  translate 0 hoverIconHeight

  svgRender $ case x of
    Just (_, CollapseIcon) -> hoverCollapseSVG
    _                      -> iconCollapseSVG

-- | Handle a mouse click. If an object was clicked an 'UpdateSignal' is sent
--   that causes the object to be evaluated and the screen to be updated.
click :: IO ()
click = do
  s <- readIORef state

  hm <- inHistoryMode
  when (not hm) $ case iconHover s of
    Nothing -> case hover s of
      -- This might fail when a click occurs during an update
      Node t -> evaluateClick s t
      _ -> return ()

    Just (Node t, EvaluateIcon) -> evaluateClick s t
    Just (Node t, CollapseIcon) -> collapseClick s t
    _ -> return ()
  when hm $ case iconHover s of
    -- Don't evaluate when we're back in time, but allow collapsing
    Just (Node t, CollapseIcon) -> collapseClick s t
    _ -> return ()

evaluateClick :: State -> Int -> IO ()
evaluateClick s t = unless (length (boxes s) <= t) $ do
  evaluate2 $ boxes s !! t
  -- Without forkIO it would hang indefinitely if some action is currently
  -- executed
  void $ forkIO $ putMVar visSignal UpdateSignal

collapseClick :: State -> Int -> IO ()
collapseClick s t = unless (length (boxes s) <= t) $ do
  hide $ boxes s !! t
  void $ forkIO $ putMVar visSignal RedrawSignal

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

      (mx, my) = mousePos vs

      check (o, (x,y,w,h)) =
        if x <= mx && mx <= x + w &&
           y <= my && my <= y + h
        then o else None

      check2 (o, (x,y,w,h)) =
        if x <= mx && mx <= x + w &&
           y <= my && my <= y + h
        then Just o else Nothing

      validOne (None:xs) = validOne xs
      validOne (x:_) = x
      validOne _ = None

      validOne2 (Nothing:xs) = validOne2 xs
      validOne2 (Just x:_) = Just x
      validOne2 _ = Nothing

  let iconHov = case oldHover of
        Node n -> validOne2 $ map check2 $ fromJust $ lookup (Node n) $ hoverIconBounds oldS
        _      -> Nothing

  case iconHov of
    Just i -> do
      let ih = Just (oldHover, i)
      modifyIORef state $ \s' -> s' {iconHover = ih}
      unless (iconHover oldS == ih) $ widgetQueueDraw canvas

    Nothing -> do
      let h = validOne $ map check $ bounds oldS
      modifyIORef state $ \s' -> s' {hover = h, iconHover = Nothing}
      unless (oldHover == h && iconHover oldS == Nothing) $ widgetQueueDraw canvas

-- | Something might have changed on the heap, update the view.
updateObjects :: [NamedBox] -> IO ()
updateObjects _boxes = do
  hidden <- readMVar visHidden
  (ops, bs', _ , size) <- xDotParse $ hidden

  modifyIORef state (\s -> s {operations = ops, boxes = bs', totalSize = size, hover = None})
