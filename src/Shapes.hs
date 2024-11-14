module Shapes(
  Matrix(..), Shape(..), Vector(..), Point, Transformation(..), Drawing(..), Colour,
  circle, square, rectangle, ellipse, polygon,
  -- scale, rotate, shear, translate -- TODO implement shear and rotate
  scale, scaleX, scaleY, scale1, scale2, translate, -- remove later
  shear, shearX, shearY, shear1, shear2,
  -- over, under, left, right, above, below, xor
  black, white, red, green, blue, colour,
  blank, background, shapeToDrawing, over,
  above,

  rotate,

  fill,

  matrix,

  point
) where

import Codec.Picture

data Vector = Vector Double Double
  deriving Show

data Matrix = Matrix Vector Vector
  deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

-- TODO have this at all?
type Point = Vector

point :: Double -> Double -> Point
point = Vector

getX (Vector x _) = x
getY (Vector _ y) = y

-- TODO could be ints maybe?
type Colour = PixelRGB8

data Transformation = Scale Double Double
                    | Rotate Matrix
                    | Shear Double Double
                    | Translate Double Double

data Shape = Square Colour
           | Circle Colour
           | Polygon Colour [Point]
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
over :: Shape -> Drawing -> Drawing
s `over` d = Over s d

above :: Shape -> Drawing -> Drawing
s `above` d = Above s d

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
-- s `shear` (x, y) = Transform (Shear x y) s
(Transform ts s) `shear` (x, y) = Transform (Shear x y : ts) s

-- Translate functions
translate :: Shape -> (Double, Double) -> Shape
-- translate s (x, y) = Transform (Translate x y) s
translate (Transform ts s) (x, y) = Transform (Translate x y : ts) s

-- Rotate functions
rotate :: Shape -> Double -> Shape
-- s `rotate` angle = Transform (Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)) s
(Transform ts s) `rotate` angle = Transform ((Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)) : ts) s