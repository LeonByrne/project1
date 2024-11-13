module Server(
  example1,
  example2
) where

import Codec.Picture
import Data.ByteString.Lazy (ByteString)

import Shapes
import Render

-- Just a little helper for scotty
imgToStr :: Image PixelRGB8 -> ByteString
imgToStr img = encodePng img

-- Below are a number of funtions that return shapes to show

example1 :: ByteString
example1 = imgToStr img
  where drawing = (circle `fill` blue) `over` background white
        img = render1 drawing (1000, 1000)

example2 :: ByteString
example2 = imgToStr img
  where drawing = (square `fill` green `scale1` 0.5 `rotate` 45) `over` background red
        img = render1 drawing (1000, 1000)