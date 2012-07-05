module GHC.Vis.GTK (
  tvis,
  vis,
  Signal(..),
  visGlobalVar,
  --visGlobalBoxes,
  --visRunning,
  gtkBprint,
  gtkDprint,
  gtkEval,
  )
  where
import Graphics.UI.Gtk hiding (Box, Signal)
import Graphics.Rendering.Cairo

import Control.Concurrent
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad

import Data.List
import qualified Data.Map as Map

import System.IO.Unsafe
import System.Timeout

import GHC.Vis
import GHC.HeapView

fontSize = 15

visRunning = unsafePerformIO (newMVar False :: IO (MVar Bool))
visGlobalVar = unsafePerformIO (newEmptyMVar :: IO (MVar GHC.Vis.GTK.Signal))
visGlobalBoxes = unsafePerformIO (newMVar [] :: IO (MVar [Box]))

gtkBprint a = do
  bs <- readMVar visGlobalBoxes
  case findIndex (asBox a ==) bs of
    Just pos -> do
      t  <- fmparse bs
      return $ show (t !! pos)
    Nothing -> return "Add entry first"

gtkDprint = do
  bs <- readMVar visGlobalBoxes
  (t,(_,h)) <- fhmparse bs
  return (show t, h)

gtkEval name = do (_,hm) <- gtkDprint
                  (show $ map go hm) `deepseq` return ()
  where go ((Box a),(Just n, y)) | n == name = seq a (Just n, y)
                                 | otherwise = (Just n, y)
        go (_,(x,y)) = (x,y)

data Signal = NewSignal Box
            | UpdateSignal
            | ClearSignal

tvis = do
  vr <- swapMVar visRunning True
  case vr of
    False -> forkIO vis >> return ()
    True  -> return ()

vis = do
  initGUI
  window <- windowNew
  --onDestroy window mainQuit -- Causes :r problems with multiple windows

  canvas <- drawingAreaNew

  set window [ windowTitle := "Vis"
             , containerChild := canvas
             ]

  onExpose canvas $ const $ do
    redraw canvas
    return True

  widgetShowAll window

  reactThread <- forkIO $ react canvas window
  onDestroy window (quit reactThread)

  mainGUI
  --swapMVar visRunning False
  return ()

quit reactThread = do
  swapMVar visRunning False
  killThread reactThread


react canvas window = do
  -- Timeout used to handle ghci reloads (:r)
  -- Reloads cause the visGlobalVar to be reinitialized, but takeMVar is still
  -- waiting for the old one.  This solution is not perfect, but it works for
  -- now.
  mbSignal <- timeout 1000000 (takeMVar visGlobalVar)
  case mbSignal of
    Nothing -> do
      running <- readMVar visRunning
      case running of
        -- :r caused this
        False -> swapMVar visRunning True >> putMVar visGlobalVar UpdateSignal >> react canvas window
        True -> react canvas window
    Just signal -> do
      case signal of
        NewSignal x  -> modifyMVar_ visGlobalBoxes (\y -> if elem x y then return y else return $ y ++ [x])
        ClearSignal  -> modifyMVar_ visGlobalBoxes (\_ -> return [])
        UpdateSignal -> return ()

      threadDelay 10000 -- 10 ms, else sometimes redraw happens too fast (Why?)
      widgetQueueDraw canvas
      react canvas window

redraw canvas = do
  boxes <- readMVar visGlobalBoxes
  --heapMap <- mWalkHeap boxes
  --let texts = fmmp boxes heapMap
  texts <- fmparse boxes
  --texts <- mapM (\(Box a) -> bprint a) boxes
  render canvas $ do
    pos <- mapM (\text -> height text) texts
    let rpos = scanl (\a b -> a + b + 30) 30 pos
    mapM (drawEntry) (zip texts rpos)
    --setSourceRGB 0 0 0
    --moveTo 10 10
    --showText $ show texts

render canvas r = do
        win <- widgetGetDrawWindow canvas
        renderWithDrawable win $ do
          selectFontFace "DejaVu Sans" FontSlantNormal FontWeightNormal
          setFontSize fontSize
          r

drawEntry (text,pos) = do
  --save
  --translate 10 30
  --mapM draw [Unnamed "'f':'o':'o':asdasda", Link "testistaojsdlSADm"]
  --restore
  --save
  --translate 10 130
  --mapM draw [Named "t0" [Unnamed "'f':'o':'o':asdasda", Link "testistaojsdlSADm"]]
  --restore
  --save
  --translate 10 230
  --mapM draw [Named "t1" [Named "t0" [Unnamed "'f':'o':'o':asdasda", Link "testistaojsdlSADm"]]]
  --restore
  --save
  --translate 10 330
  --mapM draw [Named "t1" [Link "t1", Unnamed "foba2", Named "t0" [Unnamed "'f':'o':'o':asdasda", Link "testistaojsdlSADm"]]]
  --restore
  --save
  --translate 10 430
  --mapM draw [Named "t1" [Unnamed "1"], Unnamed ":"]
  --restore
  --save
  --translate 10 530
  --mapM draw [Named "t1" [Unnamed "1"],Unnamed ":",Function "t2",Unnamed "(",Function "t3",Unnamed "(5,",Named "t5" [Unnamed "1"],Unnamed "),",Link "t1",Unnamed ",",Link "t5",Unnamed ")"]
  --restore
  --mapM draw [Named "t0" [Unnamed "'f':'o':'o':", Link "t0"]]
  save
  translate 10 pos
  mapM draw text
  restore

height xs = do
  FontExtents fa fd fh fmx fmy <- fontExtents
  let go (Named _ ys) = (fh + 15) + (maximum $ map go ys)
      go (Unnamed _)  = fh
      go (Link _)     = (fh + 10)
      go (Function _)     = (fh + 10)
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

draw (Unnamed content) = do
  let padding = 5
  wc <- width (Unnamed content)
  moveTo (padding/2) 0
  TextExtents xb yb w h xa ya <- textExtents content
  setSourceRGB 0 0 0
  showText content
  translate wc 0

draw (Function target) = do
  moveTo 0 0
  let padding = 5
  TextExtents xb yb w h xa ya <- textExtents target
  FontExtents fa fd fh fmx fmy <- fontExtents
  wc <- width (Function target)

  let (ux, uy, uw, uh) =
        (  0
        ,  (-fa) -  padding
        ,  wc
        ,  fh   +  10
        )

  setLineCap LineCapRound
  roundedRect ux uy uw uh
  setSourceRGB 1 0.5 0.5
  fillPreserve
  setSourceRGB 0 0 0
  stroke

  moveTo padding 0
  showText target
  translate wc 0

draw (Link target) = do
  moveTo 0 0
  let padding = 5
  TextExtents xb yb w h xa ya <- textExtents target
  FontExtents fa fd fh fmx fmy <- fontExtents
  wc <- width (Link target)

  let (ux, uy, uw, uh) =
        (  0
        ,  (-fa) -  padding
        ,  wc
        ,  fh   +  10
        )

  setLineCap LineCapRound
  roundedRect ux uy uw uh
  setSourceRGB 0.5 0.5 1
  fillPreserve
  setSourceRGB 0 0 0
  stroke

  moveTo padding 0
  showText target
  translate wc 0

draw (Named name content) = do
  moveTo 0 0
  let padding = 5
  TextExtents xb _ _ _ xa _ <- textExtents name
  FontExtents fa fd fh fmx fmy <- fontExtents
  hc <- height content
  wc <- width (Named name content)

  --translate 100 30

  let (ux, uy, uw, uh) =
        ( 0
        , -fa - padding
        , wc
        , fh + 10 + hc
        )

  --setLineWidth 10
  setLineCap LineCapRound
  roundedRect ux uy uw uh
  setSourceRGB 0.5 1 0.5
  fillPreserve
  setSourceRGB 0 0 0
  stroke
  moveTo ux (hc + 5 - fa - padding)
  lineTo (ux + uw) (hc + 5 - fa - padding)
  stroke
  save
  --moveTo (150 + 1.5 * padding - (w/2)) (hc/2 + padding)
  --showText $ show content
  translate padding 0
  mapM draw content
  --TextExtents nxb nyb nw nh nxa nya <- textExtents "TEST"
  --moveTo nxa 0
  --lineTo nxa 100
  --stroke
  restore
  moveTo (uw/2 - (xa - xb)/2) (hc + 7.5 - padding)
  showText name
  translate wc 0

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
