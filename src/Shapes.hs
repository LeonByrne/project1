module Shapes(
  Shape(..), Vector(..), Point, Transformation(..), Drawing(..), Colour,
  circle, square, rectangle, ellipse, polygon,
  -- scale, rotate, shear, translate -- TODO implement shear and rotate
  scale, scaleX, scaleY, scale1, scale2, translate, -- remove later
  shear, shearX, shearY, shear1, shear2,
  -- over, under, left, right, above, below, xor
  black, white, red, green, blue, colour,
  blank, over,

  fill,

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
           | Transform Transformation Shape

data Drawing = Shape Shape
             | Over Shape Drawing
             | Left Shape Drawing
             | Right Shape Drawing
             | Above Shape Drawing
             | Below Shape Drawing

white, black, red, green, blue :: Colour
white = PixelRGB8 255 255 255
black = PixelRGB8 0 0 0
red   = PixelRGB8 255 0 0
green = PixelRGB8 0 255 0
blue  = PixelRGB8 0 0 255

colour :: Int -> Int -> Int -> Colour
colour r g b = PixelRGB8 r' g' b'
  where r' = fromIntegral r
        g' = fromIntegral g
        b' = fromIntegral b

-- Changes the colour of a shape
fill :: Shape -> Colour -> Shape
fill (Square _) c      = Square c
fill (Circle _) c      = Circle c
fill (Polygon _ ps) c  = Polygon c ps
fill (Transform t s) c = Transform t (fill s c)


blank :: Drawing
blank = Shape (square `fill` black)

over :: Shape -> Drawing -> Drawing
s `over` d = Over s d

-- Constructors for unit shapes, polygon excluded. All white
square, rectangle, ellipse, circle :: Shape
square     = Square white
rectangle  = Square white
circle     = Circle white
ellipse    = Circle white

polygon :: [Point] -> Shape
polygon points = Polygon white points

scaleX, scaleY, scale1 :: Shape -> Double -> Shape
s `scaleX` x = s `scale` (x, 1)
s `scaleY` y = s `scale` (1, y)
s `scale1` a = s `scale` (a, a)

scale2 :: Shape -> Double -> Double -> Shape
scale2 s x y = s `scale` (x, y) 

scale :: Shape -> (Double, Double) -> Shape
s `scale` (x, y) = Transform (Scale x y) s

shearX, shearY, shear1 :: Shape -> Double -> Shape
s `shearX` x = s `shear` (x, 0)
s `shearY` y = s `shear` (0, y)
s `shear1` a = s `shear` (a, a)

shear2 :: Shape -> Double -> Double -> Shape
shear2 s x y = s `shear` (x, y)

shear :: Shape -> (Double, Double) -> Shape
s `shear` (x, y) = Transform (Shear x y) s

translate :: Shape -> (Double, Double) -> Shape
translate s (x, y) = Transform (Translate x y) s
