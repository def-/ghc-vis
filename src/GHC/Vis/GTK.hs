module GHC.Vis.GTK (
  visualization,
  Signal(..),
  visSignal,
  printOne,
  printAll,
  evaluate
  )
  where
import Graphics.UI.Gtk hiding (Box, Signal)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events as E

import Control.Concurrent
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad

import Data.List
import Data.IORef

import System.IO.Unsafe
import System.Timeout

import GHC.Vis
import GHC.Vis.Graph hiding (width)
import GHC.HeapView

fontSize = 15
padding = 5

data Signal = NewSignal Box -- Add a new Box to be visualized
            | UpdateSignal  -- Redraw
            | ClearSignal   -- Remove all Boxes

-- Communication channel to the visualization
visSignal = unsafePerformIO (newEmptyMVar :: IO (MVar Signal))

-- Whether a visualization is currently running
visRunning = unsafePerformIO (newMVar False)

visState = unsafePerformIO $ newIORef $ State [] [] [] [] [] (0,0) Nothing Nothing

-- All the visualized boxes
visBoxes = unsafePerformIO (newMVar [] :: IO (MVar [Box]))

-- All objects on the screen and their positions (x,y) and size (w,h)
--visPositions = unsafePerformIO (newMVar [] :: IO (MVar [(VisObject, (Double, Double, Double, Double))]))

printOne a = do
  bs <- readMVar visBoxes
  case findIndex (asBox a ==) bs of
    Just pos -> do
      t  <- parseBoxes bs
      return $ show (t !! pos)
    Nothing -> return "Add entry first"

printAll = do
  bs <- readMVar visBoxes
  (t,(_,h)) <- parseBoxesHeap bs
  return (show t, h)

evaluate identifier = do (_,hm) <- printAll
                         (show $ map go hm) `deepseq` return ()
  where go ((Box a),(Just n, y)) | n == identifier = seq a (Just n, y)
                                 | otherwise = (Just n, y)
        go (_,(x,y)) = (x,y)

evaluate2 (Box a) = a `seq` return ()

visualization = do
  vr <- swapMVar visRunning True
  case vr of
    False -> forkIO visMainThread >> return ()
    True  -> return ()

visMainThread = do
  initGUI
  window <- windowNew

  canvas <- drawingAreaNew

  set window [ windowTitle := "Vis"
             , containerChild := canvas
             ]

  onExpose canvas $ const $ do
    redraw canvas
    return True

  onMotionNotify canvas False $ \e -> do
    modifyIORef visState (\s -> s {mousePos = (E.eventX e, E.eventY e)})
    tick canvas
    return True

  onButtonPress canvas $ \e -> do
    when (E.eventButton e == LeftButton && E.eventClick e == SingleClick) $
      click
    return True

  widgetShowAll window

  reactThread <- forkIO $ react canvas window
  --onDestroy window mainQuit -- Causes :r problems with multiple windows
  onDestroy window (quit reactThread)

  mainGUI
  return ()

click = do
  s <- readIORef visState
  --case hover s of
  --  Just t -> do
  --    --seq a (return ())
  --    evaluate t
  --    putMVar visSignal UpdateSignal
  --  _ -> return ()
  case hover2 s of
    Just t -> do
      --seq a (return ())
      evaluate2 $ (boxes2 s) !! t
      putMVar visSignal UpdateSignal
    _ -> return ()

tick canvas = do
  s <- readIORef visState
  --let oldHover = hover s
  let oldHover2 = hover2 s
  modifyIORef visState $ \s -> (
    let (mx, my) = mousePos s
        check (o, (x,y,w,h)) =
          if x <= mx && mx <= x + w &&
             y <= my && my <= y + h
          then Just o else Nothing
    --in s {hover = msum $ map check (bounds s)}
    in s {hover2 = msum $ map check (bounds2 s)}
    )
  s <- readIORef visState
  if oldHover2 == hover2 s then return () else widgetQueueDraw canvas

quit reactThread = do
  swapMVar visRunning False
  killThread reactThread

react canvas window = do
  -- Timeout used to handle ghci reloads (:r)
  -- Reloads cause the visSignal to be reinitialized, but takeMVar is still
  -- waiting for the old one.  This solution is not perfect, but it works for
  -- now.
  mbSignal <- timeout 1000000 (takeMVar visSignal)
  case mbSignal of
    Nothing -> do
      running <- readMVar visRunning
      case running of
        -- :r caused visRunning to be reset
        False -> do
          swapMVar visRunning True
          putMVar visSignal UpdateSignal
          react canvas window
        True -> react canvas window
    Just signal -> do
      case signal of
        NewSignal x  -> modifyMVar_ visBoxes (
          \y -> if elem x y then return y else return $ y ++ [x])
        ClearSignal  -> modifyMVar_ visBoxes (\_ -> return [])
        UpdateSignal -> return ()

      boxes <- readMVar visBoxes
      objs <- parseBoxes boxes
      modifyIORef visState (\s -> s {objects = objs})

      -- Doesn't seem to happen anymore:
      --threadDelay 10000 -- 10 ms, else sometimes redraw happens too fast

      widgetQueueDraw canvas
      react canvas window

redraw canvas = do
  boxes <- readMVar visBoxes
  --objects <- parseBoxes boxes

  s <- readIORef visState
  --let objs = objects s
  --let h = hover s
  (ops, boxes2, size@(sx,sy,sw,sh)) <- op boxes
  Rectangle rx ry rw rh <- widgetGetAllocation canvas

  boundingBoxes <- render canvas $ do
    save
    translate (0.5 * (fromIntegral rw)) (0.5 * (fromIntegral rh))
    --scale ((fromIntegral rx)/sx) ((fromIntegral ry)/sy)

    --pos <- mapM height objs
    --let rpos = scanl (\a b -> a + b + 30) 30 pos
    result <- drawAll s size ops
    --mapM (drawEntry s) (zip objs rpos)

    restore
    return result
  return ()
  --modifyIORef visState (\s -> s {bounds = concat boundingBoxes})
  modifyIORef visState (\s -> s {boxes2 = boxes2})
  modifyIORef visState (\s -> s {bounds2 = boundingBoxes})

render canvas r = do
        win <- widgetGetDrawWindow canvas
        renderWithDrawable win $ do
          selectFontFace "DejaVu Sans" FontSlantNormal FontWeightNormal
          setFontSize fontSize
          r

drawEntry s (obj, pos) = do
  save
  translate 10 pos
  moveTo 0 0
  boundingBoxes <- mapM (draw s) obj
  restore
  return $ map (\(o, (x,y,w,h)) -> (o, (x+10,y+pos,w,h))) $ concat boundingBoxes

draw _ o@(Unnamed content) = do
  (x,y) <- getCurrentPoint
  wc <- width o
  moveTo (x + padding/2) 0
  TextExtents xb yb w h xa ya <- textExtents content
  setSourceRGB 0 0 0
  showText content
  --translate wc 0
  moveTo (x + wc) 0

  return []

draw s o@(Function target) = do
  --moveTo 0 0
  (x,y) <- getCurrentPoint
  TextExtents xb yb w h xa ya <- textExtents target
  FontExtents fa fd fh fmx fmy <- fontExtents
  wc <- width o

  let (ux, uy, uw, uh) =
        (  x
        ,  (-fa) -  padding
        ,  wc
        ,  fh   +  10
        )

  setLineCap LineCapRound
  roundedRect ux uy uw uh

  case hover s of
    Just t -> if t == target then setSourceRGB 1 0 0 else setSourceRGB 1 0.5 0.5
    _ -> setSourceRGB 1 0.5 0.5

  fillPreserve
  setSourceRGB 0 0 0
  stroke

  moveTo (x + padding) 0
  showText target
  --translate wc 0
  moveTo (x + wc) 0

  return [(target, (ux, uy, uw, uh))]

draw s o@(Link target) = do
  --moveTo 0 0
  (x,y) <- getCurrentPoint
  TextExtents xb yb w h xa ya <- textExtents target
  FontExtents fa fd fh fmx fmy <- fontExtents
  wc <- width o

  let (ux, uy, uw, uh) =
        (  x
        ,  (-fa) -  padding
        ,  wc
        ,  fh   +  10
        )

  setLineCap LineCapRound
  roundedRect ux uy uw uh

  case hover s of
    Just t -> if t == target then setSourceRGB 0 0 1 else setSourceRGB 0.5 0.5 1
    _ -> setSourceRGB 0.5 0.5 1

  fillPreserve
  setSourceRGB 0 0 0
  stroke

  moveTo (x + padding) 0
  showText target
  --translate wc 0
  moveTo (x + wc) 0

  return [(target, (ux, uy, uw, uh))]

draw s o@(Named name content) = do
  --moveTo 0 0
  (x,y) <- getCurrentPoint
  TextExtents xb _ _ _ xa _ <- textExtents name
  FontExtents fa fd fh fmx fmy <- fontExtents
  hc <- height content
  wc <- width o

  let (ux, uy, uw, uh) =
        ( x
        , -fa - padding
        , wc
        , fh + 10 + hc
        )

  setLineCap LineCapRound
  roundedRect ux uy uw uh

  case hover s of
    Just t -> if t == name then setSourceRGB 0 1 0 else setSourceRGB 0.5 1 0.5
    _ -> setSourceRGB 0.5 1 0.5

  fillPreserve
  setSourceRGB 0 0 0
  stroke
  moveTo ux (hc + 5 - fa - padding)
  lineTo (ux + uw) (hc + 5 - fa - padding)
  stroke

  save
  --translate padding 0
  moveTo (x + padding) 0
  bb <- mapM (draw s) content
  restore

  moveTo (x + uw/2 - (xa - xb)/2) (hc + 7.5 - padding)
  showText name
  --translate wc 0
  moveTo (x + wc) 0

  return $ concat bb ++ [(name, (ux, uy, uw, uh))]

roundedRect x y w h = do
  moveTo       x            (y + pad)
  lineTo       x            (y + h - pad)
  arcNegative (x + pad)     (y + h - pad) pad pi      (pi/2)
  lineTo      (x + w - pad) (y + h)
  arcNegative (x + w - pad) (y + h - pad) pad (pi/2)  0
  lineTo      (x + w)       (y + pad)
  arcNegative (x + w - pad) (y + pad)     pad 0       (-pi/2)
  lineTo      (x + pad)      y
  arcNegative (x + pad)     (y + pad)     pad (-pi/2) (-pi)
  closePath

  where pad = 1/10 * min w h

height xs = do
  FontExtents fa fd fh fmx fmy <- fontExtents
  let go (Named _ ys) = (fh + 15) + (maximum $ map go ys)
      go (Unnamed _)  = fh
      go (Link _)     = (fh + 10)
      go (Function _) = (fh + 10)
  return $ maximum $ map go xs

width (Named x ys) = do
  TextExtents xb _ _ _ xa _ <- textExtents x
  w2s <- mapM width ys
  return $ (max (xa - xb) (sum w2s)) + 10

width (Unnamed x) = do
  TextExtents xb _ _ _ xa _ <- textExtents x
  return $ (xa - xb) + 10

width (Link x) = do
  TextExtents xb _ _ _ xa _ <- textExtents x
  return $ xa - xb + 10

width (Function x) = do
  TextExtents xb _ _ _ xa _ <- textExtents x
  return $ xa - xb + 10
