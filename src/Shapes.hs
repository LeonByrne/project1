module Shapes(
  Matrix(..), Shape(..), Point, Transformation(..), Drawing(..), Colour,
  circle, square, rectangle, ellipse, polygon,
  black, white, red, green, blue, colour,
  blank, background, shapeToDrawing, 
  over, left, right, above, below,

  scale, scaleX, scaleY, scale1, scale2,
  shear, shearX, shearY, shear1, shear2,
  rotate,
  translate,

  fill,

  matrix
) where

import Prelude hiding(Left, Right)

import Codec.Picture

data Matrix = Matrix Double Double Double Double
  deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix a b c d

type Point = (Double, Double)

-- TODO could be ints maybe?
type Colour = PixelRGB8

data Transformation = Scale Double Double
                    | Rotate Matrix
                    | Shear Double Double
                    | Translate Double Double

data Shape = Square    Colour
           | Circle    Colour
           | Polygon   Colour [Point]
           | Transform [Transformation] Shape

data Drawing = Shape {shape :: Shape}
             | Over  {shape :: Shape, drawing :: Drawing}
             | Left  {shape :: Shape, drawing :: Drawing}
             | Right {shape :: Shape, drawing :: Drawing}
             | Above {shape :: Shape, drawing :: Drawing}
             | Below {shape :: Shape, drawing :: Drawing}

-- Basic colours I think are useful
white, black, red, green, blue :: Colour
white = PixelRGB8 255 255 255
black = PixelRGB8 0 0 0
red   = PixelRGB8 255 0 0
green = PixelRGB8 0 255 0
blue  = PixelRGB8 0 0 255

-- Define your a=own colours
colour :: Int -> Int -> Int -> Colour
colour r g b = PixelRGB8 r' g' b'
  where r' = fromIntegral r
        g' = fromIntegral g
        b' = fromIntegral b

-- Changes the colour of a shape, can be applied any time
fill :: Shape -> Colour -> Shape
fill (Square _) c      = Square c
fill (Circle _) c      = Circle c
fill (Polygon _ ps) c  = Polygon c ps
fill (Transform t s) c = Transform t (fill s c)

-- Returns a drawing that is just a black square
blank :: Drawing
blank = Shape (square `fill` black)

-- Returns a Drawing that is just a square of specified colour
background :: Colour -> Drawing
background c = Shape (square `fill` c)

shapeToDrawing :: Shape -> Drawing
shapeToDrawing s = Shape s

-- Draw a shape over a drawing
over, left, right, above, below :: Shape -> Drawing -> Drawing
s `over` d  = Over s d
s `left` d  = Left s d
s `right` d = Right s d
s `above` d = Above s d
s `below` d = Below s d

-- Constructors for unit shapes, polygon excluded. All white
square, rectangle, ellipse, circle :: Shape
square     = Square white
rectangle  = Square white
circle     = Circle white
ellipse    = Circle white

polygon :: [Point] -> Shape
polygon points = Polygon white points

-- Scale functions
scaleX, scaleY, scale1 :: Shape -> Double -> Shape
s `scaleX` x = s `scale` (x, 1)
s `scaleY` y = s `scale` (1, y)
s `scale1` a = s `scale` (a, a)

scale2 :: Shape -> Double -> Double -> Shape
scale2 s x y = s `scale` (x, y) 

scale :: Shape -> (Double, Double) -> Shape
-- s `scale` (x, y) = Transform (Scale x y) s
(Transform ts s) `scale` (x, y) = Transform (Scale x y : ts) s
s `scale` (x, y) = Transform [Scale x y] s

-- Shear functions
shearX, shearY, shear1 :: Shape -> Double -> Shape
s `shearX` x = s `shear` (x, 0)
s `shearY` y = s `shear` (0, y)
s `shear1` a = s `shear` (a, a)

shear2 :: Shape -> Double -> Double -> Shape
shear2 s x y = s `shear` (x, y)

shear :: Shape -> (Double, Double) -> Shape
(Transform ts s) `shear` (x, y) = Transform (Shear x y : ts) s
s `shear` (x, y) = Transform [Shear x y] s

-- Translate functions
translate :: Shape -> (Double, Double) -> Shape
translate (Transform ts s) (x, y) = Transform (Translate x y : ts) s
translate s (x, y) = Transform [Translate x y] s

-- Rotate functions
rotate :: Shape -> Double -> Shape
(Transform ts s) `rotate` angle = Transform ((Rotate $ matrix (cos angle') (-sin angle') (sin angle') (cos angle')) : ts) s
  where angle' = angle * pi / 180
s `rotate` angle = Transform [(Rotate $ matrix (cos angle') (-sin angle') (sin angle') (cos angle'))] s
  where angle' = angle * pi / 180