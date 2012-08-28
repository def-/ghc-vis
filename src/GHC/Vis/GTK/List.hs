module GHC.Vis.GTK.List (
  redraw,
  click,
  tick,
  updateObjects
  )
  where
import Graphics.UI.Gtk hiding (Box, Signal)
import Graphics.Rendering.Cairo

import Control.Concurrent
import Control.Monad

import Data.IORef

import GHC.Vis.Internal hiding (boxes)
import GHC.Vis.Types hiding (boxes)
import GHC.Vis.GTK.Common

import GHC.HeapView (Box)

padding :: Double
padding = 5

fontSize :: Double
fontSize = 15

redraw :: WidgetClass w => w -> IO ()
redraw canvas = do
  boxes <- readMVar visBoxes

  s <- readIORef visState
  Rectangle _ _ rw rh <- widgetGetAllocation canvas

  let objs = objects s

  boundingBoxes <- render canvas $ do
    let names = map ((++ ": ") . snd) boxes
    nameWidths <- mapM (width . Unnamed) names
    let maxNameWidth = maximum nameWidths

    pos <- mapM height objs

    widths <- mapM (mapM width) objs
    let widths2 = 1 : map (\ws -> maxNameWidth + sum ws) widths

    let sw = maximum widths2
    let sh = sum (map (+ 30) pos) - 15

    let scalex = min (fromIntegral rw / sw) (fromIntegral rh / sh)
        scaley = scalex
        offsetx = 0
        offsety = 0
    save
    translate offsetx offsety
    scale scalex scaley

    let rpos = scanl (\a b -> a + b + 30) 30 pos
    result <- mapM (drawEntry s maxNameWidth) (zip3 objs rpos names)

    restore
    --return result
    return $ map (\(o, (x,y,w,h)) -> (o, (x*scalex+offsetx,y*scaley+offsety,w*scalex,h*scaley))) $ concat result
  modifyIORef visState (\s' -> s' {bounds = boundingBoxes})

render :: WidgetClass w => w -> Render b -> IO b
render canvas r = do
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do
    selectFontFace "DejaVu Sans" FontSlantNormal FontWeightNormal
    setFontSize fontSize
    r

  --Rectangle _ _ rw rh <- widgetGetAllocation canvas
  --withSVGSurface "export.svg" (fromIntegral rw) (fromIntegral rh) (\s -> renderWith s r)

click :: IO ()
click = do
  s <- readIORef visState

  case hover s of
     Just t -> do
       evaluate t
       putMVar visSignal UpdateSignal
     _ -> return ()

tick :: WidgetClass w => w -> IO ()
tick canvas = do
  oldS <- readIORef visState
  let oldHover = hover oldS

  modifyIORef visState $ \s' -> (
    let (mx, my) = mousePos s'
        check (o, (x,y,w,h)) =
          if x <= mx && mx <= x + w &&
             y <= my && my <= y + h
          then Just o else Nothing
    in s' {hover = msum $ map check (bounds s')}
    )
  s <- readIORef visState
  unless (oldHover == hover s) $ widgetQueueDraw canvas

updateObjects :: [(Box, String)] -> IO ()
updateObjects boxes = do
  objs <- parseBoxes boxes
  modifyIORef visState (\s -> s {objects = objs})

drawEntry :: State -> Double -> ([VisObject], Double, String) -> Render [(String, (Double, Double, Double, Double))]
drawEntry s nameWidth (obj, pos, name) = do
  save
  translate 0 pos
  moveTo 0 0
  draw s $ Unnamed name
  --setSourceRGB 0 0 0
  --showText name
  translate nameWidth 0
  moveTo 0 0
  boundingBoxes <- mapM (draw s) obj
  restore
  return $ map (\(o, (x,y,w,h)) -> (o, (x+nameWidth,y+pos,w,h))) $ concat boundingBoxes

draw :: State -> VisObject -> Render [(String, (Double, Double, Double, Double))]
draw _ o@(Unnamed content) = do
  (x,_) <- getCurrentPoint
  wc <- width o
  moveTo (x + padding/2) 0
  setSourceRGB 0 0 0
  showText content
  --translate wc 0
  moveTo (x + wc) 0

  return []

draw s o@(Function target) =
  drawFunctionLink s o target (1,0,0) (1,0.5,0.5)

draw s o@(Link target) =
  drawFunctionLink s o target (0,0,1) (0.5,0.5,1)

draw s o@(Named name content) = do
  (x,_) <- getCurrentPoint
  TextExtents xb _ _ _ xa _ <- textExtents name
  FontExtents fa _ fh _ _ <- fontExtents
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

  fillAndSurround

  moveTo ux (hc + 5 - fa - padding)
  lineTo (ux + uw) (hc + 5 - fa - padding)
  stroke

  save
  moveTo (x + padding) 0
  bb <- mapM (draw s) content
  restore

  moveTo (x + uw/2 - (xa - xb)/2) (hc + 7.5 - padding)
  showText name
  moveTo (x + wc) 0

  return $ concat bb ++ [(name, (ux, uy, uw, uh))]

drawFunctionLink :: State -> VisObject -> String -> (Double, Double, Double) -> (Double, Double, Double) -> Render [(String, (Double, Double, Double, Double))]
drawFunctionLink s o target (r1,g1,b1) (r2,g2,b2) = do
  (x,_) <- getCurrentPoint
  FontExtents fa _ fh _ _ <- fontExtents
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
    Just t -> if t == target then setSourceRGB r1 g1 b1 else setSourceRGB r2 g2 b2
    _ -> setSourceRGB r2 g2 b2

  fillAndSurround

  moveTo (x + padding) 0
  showText target
  moveTo (x + wc) 0

  return [(target, (ux, uy, uw, uh))]

fillAndSurround :: Render ()
fillAndSurround = do
  fillPreserve
  setSourceRGB 0 0 0
  stroke

roundedRect :: Double -> Double -> Double -> Double -> Render ()
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

height :: [VisObject] -> Render Double
height xs = do
  FontExtents _ _ fh _ _ <- fontExtents
  let go (Named _ ys) = (fh + 15) + maximum (map go ys)
      go (Unnamed _)  = fh
      go (Link _)     = fh + 10
      go (Function _) = fh + 10
  return $ maximum $ map go xs

width :: VisObject -> Render Double
width (Named x ys) = do
  TextExtents xb _ _ _ xa _ <- textExtents x
  w2s <- mapM width ys
  return $ max (xa - xb) (sum w2s) + 10

width (Unnamed x) = do
  TextExtents xb _ _ _ xa _ <- textExtents x
  return $ (xa - xb) + 10

width (Link x) = do
  TextExtents xb _ _ _ xa _ <- textExtents x
  return $ xa - xb + 10

width (Function x) = do
  TextExtents xb _ _ _ xa _ <- textExtents x
  return $ xa - xb + 10
