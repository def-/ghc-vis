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

import Control.Monad

import System.IO.Unsafe

import GHC.Vis
import GHC.HeapView

fontSize = 15

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
    UpdateSignal -> putStrLn "update" >> return ()

  threadDelay 10000 -- 10 ms, else sometimes redraw happens too fast (Why?)
  widgetQueueDraw canvas
  react ref bref canvas

redraw canvas bref = do
  boxes <- readMVar bref
  heapMap <- mWalkHeap boxes
  let texts = fmmp heapMap boxes
  --texts <- mapM (\(Box a) -> bprint a) boxes
  render canvas $ do
    mapM (drawEntry) texts
    --setSourceRGB 0 0 0
    --moveTo 10 10
    --showText $ show texts

render canvas r = do
        win <- widgetGetDrawWindow canvas
        renderWithDrawable win $ do
          selectFontFace "DejaVu Sans" FontSlantNormal FontWeightNormal
          setFontSize fontSize
          r

drawEntry texts = do
  save
  translate 10 30
  mapM draw [Unnamed "'f':'o':'o':asdasda", Link "testistaojsdlSADm"]
  restore
  save
  translate 10 130
  mapM draw [Named "t0" [Unnamed "'f':'o':'o':asdasda", Link "testistaojsdlSADm"]]
  restore
  save
  translate 10 230
  mapM draw [Named "t1" [Named "t0" [Unnamed "'f':'o':'o':asdasda", Link "testistaojsdlSADm"]]]
  restore
  save
  translate 10 330
  mapM draw [Named "t1" [Link "t1", Unnamed "foba2", Named "t0" [Unnamed "'f':'o':'o':asdasda", Link "testistaojsdlSADm"]]]
  restore
  save
  translate 10 430
  mapM draw [Named "t1" [Unnamed "1"], Unnamed ":"]
  restore
  save
  translate 10 530
  mapM draw [Named "t1" [Unnamed "1"],Unnamed ":",Function "t2",Unnamed "(",Function "t3",Unnamed "(5,",Named "t5" [Unnamed "1"],Unnamed "),",Link "t1",Unnamed ",",Link "t5",Unnamed ")"]
  restore
  --mapM draw [Named "t0" [Unnamed "'f':'o':'o':", Link "t0"]]
  --mapM draw texts

height xs = do
  FontExtents fa fd fh fmx fmy <- fontExtents
  let go (Named _ ys) = (fh + 15) + (maximum $ map go ys)
      go (Unnamed _)  = fh
      go (Link _)     = (fh + 10)
      go (Function _)     = (fh + 10)
  return $ maximum $ map go xs

width (Named x ys) = do
  TextExtents _ _ w _ _ _ <- textExtents x
  w2s <- mapM width ys
  return $ (max w (sum w2s)) + 10

width (Unnamed x) = do
  TextExtents _ _ w _ _ _ <- textExtents x
  return $ w + 10

width (Link x) = do
  TextExtents _ _ w _ _ _ <- textExtents x
  return $ w + 20

width (Function x) = do
  TextExtents _ _ w _ _ _ <- textExtents x
  return $ w + 20

draw (Unnamed content) = do
  let padding = 5
  moveTo padding 0
  TextExtents xb yb w h xa ya <- textExtents content
  setSourceRGB 0 0 0
  showText content
  translate (xa + 2 * padding) 0

draw (Function target) = do
  moveTo 0 0
  let padding = 5
  let margin = 2
  TextExtents xb yb w h xa ya <- textExtents target
  FontExtents fa fd fh fmx fmy <- fontExtents

  let (ux, uy, uw, uh) =
        (  margin
        ,  (-fa) -  padding
        ,  w    +  2 * padding
        ,  fh   +  10
        )

  setLineCap LineCapRound
  roundedRect ux uy uw uh
  setSourceRGB 1 0 0
  fillPreserve
  setSourceRGB 0 0 0
  stroke

  moveTo (margin/2 + padding) 0
  showText target
  translate (xa + 2 * padding) 0

draw (Link target) = do
  moveTo 0 0
  let padding = 5
  let margin = 2
  TextExtents xb yb w h xa ya <- textExtents target
  FontExtents fa fd fh fmx fmy <- fontExtents

  let (ux, uy, uw, uh) =
        (  margin
        ,  (-fa) -  padding
        ,  w    +  2 * padding
        ,  fh   +  10
        )

  setLineCap LineCapRound
  roundedRect ux uy uw uh
  setSourceRGB 1 0 0
  fillPreserve
  setSourceRGB 0 0 0
  stroke

  moveTo (margin/2 + padding) 0
  showText target
  translate (xa + 2 * padding) 0

draw (Named name content) = do
  moveTo 0 0
  let padding = 5
  TextExtents _ _ w1 _ _ _ <- textExtents name
  FontExtents fa fd fh fmx fmy <- fontExtents
  hc <- height content
  wc <- width (Named name content)

  --translate 100 30

  let (ux, uy, uw, uh) =
        ( 0
        , -fa - padding
        , wc + 3 * padding
        , fh + 10 + hc
        )

  --setLineWidth 10
  setLineCap LineCapRound
  roundedRect ux uy uw uh
  setSourceRGB 1 0 0
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
  moveTo (wc/2 + 1.5 * padding - (w1/2)) (hc + 7.5 - padding)
  showText name
  translate (wc + 3 * padding) 0

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
