module Server(
  example1
) where

import Codec.Picture
import Data.ByteString.Lazy (ByteString)

import Shapes
import Render

-- Just a little helper for scotty
imgToStr :: Image PixelRGB8 -> ByteString
imgToStr img = encodePng img

example1 :: ByteString
example1 = imgToStr img
  where drawing = (circle `fill` blue) `over` blank
        img = render1 drawing (1000, 1000)