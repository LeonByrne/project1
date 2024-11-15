{-# LANGUAGE OverloadedStrings #-}

module Server(
  mainPage,

  circleExample,
  squareExample,
  rectExample, 
  ellipseExample,
  polyExample,

  eyeExample,
  snowmanExample,
  shearExample,
  rotateExample,

  -- Not used now
  example2,
  example3,
  example4,
  example5
) where

import Codec.Picture
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Data.ByteString.Lazy (ByteString)

import Shapes
import Render

-- Just a little helper for scotty
imgToStr :: Image PixelRGB8 -> ByteString
imgToStr img = encodePng img

-- Below are a number of funtions that return shapes to show

mainPage :: H.Html
mainPage = H.docTypeHtml $ do
  H.head $ do
    H.title "Showcase of drawing eDsl"
  H.body $ do
    H.h1 "See some examples of using the created eDSL below"
    H.p  "Note, the call to render is excluded as it doesn't show much"
    H.h2 "Starting with some simple blue shapes over black backgrounds. I've also scaled them down a bit"
    H.img ! A.src "/circle"
    H.p  "(circle `fill` blue `scale1` 0.5) `over` background white"
    H.img ! A.src "/square"
    H.p "(square `fill` blue `scale1` 0.5) `over` background black"
    H.img ! A.src "/polygon"
    H.p "(polygon [((-0.5), 0.5), (0.5, 0.5), (0, (-0.5))] `fill` blue `scale1` 0.5) `over` background black"
    H.img ! A.src "/rectangle"
    H.p "(rectangle `fill` blue `scale` (0.5, 0.25)) `over` background black"
    H.img ! A.src "/ellipse"
    H.p "(ellipse`fill` blue `scale` (0.5, 0.25)) `over` background black"

    H.h2 "Shapes can be altered and combined in more interesting ways."
    H.img ! A.src "/eye"
    H.p "circle `fill` black `scale1` 0.1 `over` ((circle `fill` blue `scale1` 0.2) `over` ((circle `fill` white `scale` (0.6, 0.4)) `over` background black))"
    H.img ! A.src "/snowman"
    H.p "(circle `fill` white `scale` 0.1) `above` ((circle `fill` white `scale1` 0.2) `above` shapeToDrawing (circle `fill` white `scale1` 0.4 `translateY` 0.3))"
    H.img ! A.src "/shear"
    H.p "(square `fill` blue `shear` (0.2, 0) `scale1` 0.4) `over` background black"
    H.img ! A.src "/rotate"
    H.p "(square `fill` blue `rotate` 45 `scale1` 0.4) `over` background black"


circleExample :: ByteString
circleExample = imgToStr img
  where 
    drawing = (circle `fill` blue `scale1` 0.5) `over` background black
    img = render drawing (500, 500)

squareExample :: ByteString
squareExample = imgToStr img
  where
    drawing = (square `fill` blue `scale1` 0.5) `over` background black
    img = render drawing (500, 500)

rectExample :: ByteString
rectExample = imgToStr img
  where
    drawing = (rectangle `fill` blue `scale` (0.5, 0.25)) `over` background black
    img = render drawing (500, 500)

ellipseExample :: ByteString
ellipseExample = imgToStr img
  where
    drawing = (ellipse`fill` blue `scale` (0.5, 0.25)) `over` background black
    img = render drawing (500, 500)

polyExample :: ByteString
polyExample = imgToStr img
  where
    drawing = (polygon [((-0.5), 0.5), (0.5, 0.5), (0, (-0.5))] `fill` blue `scale1` 0.5) `over` background black
    img = render drawing (500, 500)

eyeExample :: ByteString
eyeExample = imgToStr img
  where
    drawing = circle `fill` black `scale1` 0.1 `over` ((circle `fill` blue `scale1` 0.2) `over` ((ellipse `fill` white `scale` (0.6, 0.4)) `over` background black))
    img = render drawing (500, 500)

snowmanExample :: ByteString
snowmanExample = imgToStr img
  where
    drawing = (circle `fill` white `scale1` 0.1) `above` ((circle `fill` white `scale1` 0.2) `above` shapeToDrawing (circle `fill` white `scale1` 0.4))
    img = render drawing (500, 500)

shearExample :: ByteString
shearExample = imgToStr img
  where
    drawing = (square `fill` blue `shear` (0.2, 0) `scale1` 0.4) `over` background black
    img = render drawing (500, 500)

rotateExample :: ByteString
rotateExample = imgToStr img
  where
    drawing = (square `fill` blue `rotate` 45 `scale1` 0.4) `over` background black
    img = render drawing (500, 500)

example2 :: ByteString
example2 = imgToStr img
  where 
    drawing = (square `fill` green `scale1` 0.2 `rotate` 45) `over` background red
    img = render drawing (500, 500)

example3 :: ByteString
example3 = imgToStr img
  where 
    drawing = (square `fill` red `scale1` 0.35 `shear1` 0.5) `over` ((circle `fill` green `scale1` 0.9) `over` background white)
    img = render drawing (500, 500)

example4 :: ByteString
example4 = imgToStr img
  where 
    drawing = (polygon [((-0.5), 0.5), (0.5, 0.5), (0, (-0.5))] `fill` blue `scale1` 0.5) `over` background black
    img = render drawing (500, 500)

example5 :: ByteString
example5 = imgToStr img
  where
    drawing = (square `fill` blue `scale1` 0.1) `left` shapeToDrawing (circle `fill` red `scale1` 0.5)
    img = render drawing (500, 500)