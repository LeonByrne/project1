module Render where

import Shapes
import Codec.Picture

render1 :: Drawing -> (Int, Int) -> Image PixelRGB8
render1 d (w, h) = generateImage pixelRenderer w h
  where pixelRenderer x y = PixelRGB8 r g b 
        where c = colourAt d (point x y)
              r = 