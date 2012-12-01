{- Copyright © 2012, Vincent Elisha Frey. All rights reserved.
- This is open source software distributed under a MIT license.
- See the file 'LICENSE' for further information.
-}
import qualified Graphics.UI.SDL as SDL
import Graphics.Rendering.Cairo
import Foreign.Ptr ( castPtr )

black = SDL.Pixel maxBound

-- Draw a line from the top left to the bottom right.
demo1 :: Render ()
demo1 = do
  setSourceRGB 0 0 0
  moveTo 100 100
  lineTo 500 500
  stroke

-- Draw a line from the top right to the bottom left.
demo2 :: Render ()
demo2 = do
  setSourceRGB 0 0 0
  moveTo 500 100
  lineTo 100 500
  stroke

main = SDL.withInit [ SDL.InitVideo ] $ do
  screen <- SDL.setVideoMode 600 600 32 [ SDL.Resizable ]

  SDL.fillRect screen Nothing black

  pixels <- fmap castPtr $ SDL.surfaceGetPixels screen

  -- Here we test the first of the user facing functions added by the patch.
  -- Be sure to use FORMATRGB24 so Cairo and SDL don't conflict about alpha
  -- values. If you need to use alpha channel in your Cairo routines, consider
  -- using an independent Cairo surface to buffer your alpha drawing to.
  canvas <- createImageSurfaceForData pixels FormatRGB24 600 600 (600 * 4)
  renderWith canvas demo1

  -- And here is the other new function for when you want to hide the
  -- Cairo.Surface from the caller.
  withImageSurfaceForData pixels FormatRGB24 600 600 (600 * 4) $ \canvas ->
    renderWith canvas demo2

  SDL.flip screen

  idle
  where
  idle = do
    e <- SDL.waitEvent
    case e of
         SDL.Quit -> return ()
         otherwise -> idle
