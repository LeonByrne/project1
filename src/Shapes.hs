module Shapes(
  Shape, Vector(..), Point, Transformation, Drawing,
  circle, square, rectangle, ellipse, polygon,
  -- scale, rotate, shear, translate -- TODO implement shear and rotate
  scale, scaleX, scaleY, scale1, scale2, translate, -- remove later
  -- over, under, left, right, above, below, xor
  black, white, red, green, blue, colour,
  blank, over,

  inside,
  point, getX, getY, 
  colourAt
) where

import Codec.Picture

data Vector = Vector Double Double
  deriving Show

data Matrix = Matrix Vector Vector
  deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

-- Util functions
distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)

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
                    | Shear Vector
                    | Translate Double Double

data Shape = Square Colour
           | Circle Colour
           | Polygon Colour [Point]
           | Transform Transformation Shape

data Drawing = Empty
             | Shape Shape
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

blank :: Drawing
blank = Empty

over :: Shape -> Drawing -> Drawing
s `over` d = Over s d

-- Constructors for unit shapes, polygon excluded
square, rectangle, ellipse, circle :: Shape
square = Square white
rectangle = Square white
circle = Circle white
ellipse = Circle white

polygon :: [Point] -> Shape
polygon points = Polygon white points

scaleX, scaleY, scale1 :: Shape -> Double -> Shape
s `scaleX` x = s `scale` (x, 0)
s `scaleY` y = s `scale` (0, y)
s `scale1` a = s `scale` (a, a)

scale2 :: Shape -> Double -> Double -> Shape
scale2 s x y = s `scale` (x, y) 

scale :: Shape -> (Double, Double) -> Shape
s `scale` (x, y) = Transform (Scale x y) s

translate :: Shape -> (Double, Double) -> Shape
translate s (x, y) = Transform (Translate x y) s


-- Drawing functions

transform :: Point -> Transformation -> Point
transform (Vector x y) (Scale sx sy) = Vector (x / sx) (y / sy)  
transform (Vector x y) (Translate tx ty) = Vector (x - tx) (y - ty) 
transform p _ = p -- TODO implement this for shear and rotate

inside :: Point -> Shape -> Bool
p `inside` Square _ = maxnorm p <= 0.5
p `inside` Circle _ = distance p <= 0.5
p `inside` Polygon _ _ = False -- TODO implement this
p `inside` Transform t s = transform p t `inside` s

getColour :: Shape -> Colour
getColour (Square c) = c
getColour (Circle c) = c
getColour (Polygon c _) = c
getColour (Transform _ s) = getColour s

colourAt :: Drawing -> Point -> Colour
colourAt Empty _ = black -- Using as default background
colourAt (Shape s) p | p `inside` s = getColour s
                     | otherwise    = black

colourAt (Over s d) p | p `inside` s = getColour s
                      | otherwise    = colourAt d p 
