module Render(
  render1
) where

import Shapes
import Codec.Picture

render1 :: Drawing -> (Int, Int) -> Image PixelRGB8
render1 d (w, h) = generateImage pixelRenderer w h
  where 
    pixelRenderer x y = colourAt d (point x' y')
      where x' = (-0.5) + fromIntegral x / fromIntegral w
            y' = (-0.5) + fromIntegral y / fromIntegral h
  -- where (r, g, b) = colourAt d (point x y)
  --       pixelRenderer x y = PixelRGB8 r g b

    -- let (r, g, b) = (0, 0, 0)
    -- in PixelRGB8 r g b

    -- let (r, g, b) = colourAt d (point x y)
    -- in PixelRGB8 r g b

  -- where pixelRenderer x y = PixelRGB8 r g b 
  --   where (r, g, b) = colourAt d (point x y)