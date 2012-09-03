{- |
   Module      : GHC.Vis.View.List
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

 -}
module GHC.Vis.View.List (
  export,
  redraw,
  click,
  move,
  updateObjects
  )
  where
import Graphics.UI.Gtk hiding (Box, Signal, Rectangle)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo

import Control.Concurrent
import Control.Monad

import Data.IORef
import System.IO.Unsafe

import GHC.Vis.Internal
import GHC.Vis.Types hiding (State, View(..))
import GHC.Vis.View.Common

import GHC.HeapView (Box)

type Rectangle = (Double, Double, Double, Double)

data State = State
  { objects :: [[VisObject]]
  , bounds :: [(String, Rectangle)]
  , hover :: Maybe String
  }

type RGB = (Double, Double, Double)

state :: IORef State
state = unsafePerformIO $ newIORef $ State [] [] Nothing

fontName :: String
-- Cairo's internal font system doesn't detect this as a similar font to "Times
-- Roman", should switch to Pango
--fontName = "Nimbus Roman No9 L"
fontName = "DejaVu Sans"

fontSize :: Double
fontSize = 15

colorName :: RGB
colorName = (0.5,1,0.5)

colorNameHighlighted :: RGB
colorNameHighlighted = (0,1,0)

colorLink :: RGB
colorLink = (0.5,0.5,1)

colorLinkHighlighted :: RGB
colorLinkHighlighted = (0.25,0.25,1)

colorFunction :: RGB
colorFunction = (1,0.5,0.5)

colorFunctionHighlighted :: RGB
colorFunctionHighlighted = (1,0,0)

padding :: Double
padding = 5

-- | Draw visualization to screen, called on every update or when it's
--   requested from outside the program.
redraw :: WidgetClass w => w -> IO ()
redraw canvas = do
  boxes <- readMVar visBoxes
  s <- readIORef state
  Gtk.Rectangle _ _ rw2 rh2 <- widgetGetAllocation canvas

  boundingBoxes <- render canvas (draw s rw2 rh2 boxes)
  modifyIORef state (\s' -> s' {bounds = boundingBoxes})

-- | Export the visualization to an SVG file
export :: String -> IO ()
export file = do
  boxes <- readMVar visBoxes
  s <- readIORef state

  withSVGSurface file (fromIntegral xSize) (fromIntegral ySize)
    (\surface -> renderWith surface (draw s xSize ySize boxes))

  return ()

  where xSize = 500 :: Int
        ySize = 500 :: Int

draw :: State -> Int -> Int -> [(a, String)] -> Render [(String, Rectangle)]
draw s rw2 rh2 boxes = do
  let objs = objects s
      names = map ((++ ": ") . snd) boxes

  nameWidths <- mapM (width . Unnamed) names
  pos <- mapM height objs
  widths <- mapM (mapM width) objs

  -- Text sizes aren't always perfect, assume that texts may be a bit too big
  let rw = 0.97 * fromIntegral rw2
      rh = 0.99 * fromIntegral rh2

      maxNameWidth = maximum nameWidths
      widths2 = 1 : map (\ws -> maxNameWidth + sum ws) widths

      sw = maximum widths2
      sh = sum (map (+ 30) pos) - 15

      sx = min (rw / sw) (rh / sh)
      sy = sx
      ox = 0
      oy = 0

  translate ox oy
  scale sx sy

  let rpos = scanl (\a b -> a + b + 30) 30 pos
  result <- mapM (drawEntry s maxNameWidth) (zip3 objs rpos names)

  return $ map (\(o, (x,y,w,h)) -> (o, (x*sx+ox,y*sy+oy,w*sx,h*sy))) $ concat result

render :: WidgetClass w => w -> Render b -> IO b
render canvas r = do
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do
    selectFontFace fontName FontSlantNormal FontWeightNormal
    setFontSize fontSize
    r

-- | Handle a mouse click. If an object was clicked an 'UpdateSignal' is sent
--   that causes the object to be evaluated and the screen to be updated.
click :: IO ()
click = do
  s <- readIORef state

  case hover s of
     Just t -> do
       evaluate t
       putMVar visSignal UpdateSignal
     _ -> return ()

-- | Handle a mouse move. Causes an 'UpdateSignal' if the mouse is hovering a
--   different object now, so the object gets highlighted and the screen
--   updated.
move :: WidgetClass w => w -> IO ()
move canvas = do
  vS <- readIORef visState
  oldS <- readIORef state
  let oldHover = hover oldS

  modifyIORef state $ \s' -> (
    let (mx, my) = mousePos vS
        check (o, (x,y,w,h)) =
          if x <= mx && mx <= x + w &&
             y <= my && my <= y + h
          then Just o else Nothing
    in s' {hover = msum $ map check (bounds s')}
    )
  s <- readIORef state
  unless (oldHover == hover s) $ widgetQueueDraw canvas

-- | Something might have changed on the heap, update the view.
updateObjects :: [(Box, String)] -> IO ()
updateObjects boxes = do
  objs <- parseBoxes boxes
  modifyIORef state (\s -> s {objects = objs})

drawEntry :: State -> Double -> ([VisObject], Double, String) -> Render [(String, Rectangle)]
drawEntry s nameWidth (obj, pos, name) = do
  save
  translate 0 pos
  moveTo 0 0
  drawBox s $ Unnamed name
  --setSourceRGB 0 0 0
  --showText name
  translate nameWidth 0
  moveTo 0 0
  boundingBoxes <- mapM (drawBox s) obj
  restore
  return $ map (\(o, (x,y,w,h)) -> (o, (x+nameWidth,y+pos,w,h))) $ concat boundingBoxes

drawBox :: State -> VisObject -> Render [(String, Rectangle)]
drawBox _ o@(Unnamed content) = do
  (x,_) <- getCurrentPoint
  wc <- width o
  moveTo (x + padding/2) 0
  setSourceRGB 0 0 0
  showText content
  --translate wc 0
  moveTo (x + wc) 0

  return []

drawBox s o@(Function target) =
  drawFunctionLink s o target colorFunction colorFunctionHighlighted

drawBox s o@(Link target) =
  drawFunctionLink s o target colorLink colorLinkHighlighted

drawBox s o@(Named name content) = do
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

  setColor s name colorName colorNameHighlighted

  fillAndSurround

  moveTo ux (hc + 5 - fa - padding)
  lineTo (ux + uw) (hc + 5 - fa - padding)
  stroke

  save
  moveTo (x + padding) 0
  bb <- mapM (drawBox s) content
  restore

  moveTo (x + uw/2 - (xa - xb)/2) (hc + 7.5 - padding)
  showText name
  moveTo (x + wc) 0

  return $ concat bb ++ [(name, (ux, uy, uw, uh))]

drawFunctionLink :: State -> VisObject -> String -> RGB -> RGB -> Render [(String, Rectangle)]
drawFunctionLink s o target color1 color2 = do
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

  setColor s target color1 color2

  fillAndSurround

  moveTo (x + padding) 0
  showText target
  moveTo (x + wc) 0

  return [(target, (ux, uy, uw, uh))]

setColor :: State -> String -> RGB -> RGB -> Render ()
setColor s name (r,g,b) (r',g',b') = case hover s of
  Just t -> if t == name then setSourceRGB r' g' b'
                         else setSourceRGB r  g  b
  _ -> setSourceRGB r g b

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
  return $ xa - xb + 5

width (Link x) = do
  TextExtents xb _ _ _ xa _ <- textExtents x
  return $ xa - xb + 10

width (Function x) = do
  TextExtents xb _ _ _ xa _ <- textExtents x
  return $ xa - xb + 10
