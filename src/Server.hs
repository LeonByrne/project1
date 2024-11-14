module Server(
  example1,
  example2,
  example3,
  example4,
  example5
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
  where drawing = (square `fill` green `scale1` 0.2 `rotate` 45) `over` background red
        img = render1 drawing (1000, 1000)

example3 :: ByteString
example3 = imgToStr img
  where drawing = (square `fill` red `scale1` 0.35 `shear1` 0.5) `over` ((circle `fill` green `scale1` 0.9) `over` background white)
        img = render1 drawing (1000, 1000)

example4 :: ByteString
example4 = imgToStr img
  where 
    drawing = (polygon [point (-0.5) 0.5, point 0.5 0.5, point 0 (-0.5)] `fill` blue `scale1` 0.5) `over` background black
    img = render1 drawing (1000, 1000)

example5 :: ByteString
example5 = imgToStr img
  where
    drawing = (square `fill` blue `scale1` 0.2) `above` shapeToDrawing (circle `fill` red `scaleY` 0.5)
    img = render1 drawing (1000, 1000)