module Render(
  render1
) where

import Prelude hiding (Left, Right)
import Shapes
import Codec.Picture

render1 :: Drawing -> (Int, Int) -> Image PixelRGB8
render1 d (w, h) = generateImage pixelRenderer w h
  where 
    pixelRenderer x y = colourAt d (point x' y')
      where x' = (-0.5) + fromIntegral x / fromIntegral w
            y' = (-0.5) + fromIntegral y / fromIntegral h

-- Drawing functions
distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)

cross :: Vector -> Vector -> Double
cross (Vector x1 y1) (Vector x2 y2) = x1 * x2 + y1 * y2

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

inTriangle :: Point -> (Point, Point, Point) -> Bool
inTriangle p (a, b, c) = 
  let denominator = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
      alpha = ((y2 - y3) * (x - x3) + (x3 - x2) * (y - y3)) / denominator
      beta  = ((y3 - y1) * (x - x3) + (x1 - x3) * (y - y3)) / denominator
      gamma = 1 - alpha - beta
  in alpha >= 0 && beta >= 0 && gamma >= 0
    where
      (Vector x y)   = p
      (Vector x1 y1) = a
      (Vector x2 y2) = b
      (Vector x3 y3) = c

-- We want to apply transformations in the order they wer done to shapes
transform :: Point -> [Transformation] -> Point
transform p (t:ts) = transform1 (transform p ts) t
transform p [] = p

transform1 :: Point -> Transformation -> Point
transform1 (Vector x y) (Scale sx sy) = Vector (x / sx) (y / sy)  
transform1 (Vector x y) (Shear sx sy) = Vector (x + sx*y) (y + sy*x)
transform1 (Vector x y) (Translate tx ty) = Vector (x - tx) (y - ty) 
transform1 p (Rotate m) = invert m `mult` p

inside :: Point -> Shape -> Bool
p `inside` Square _ = maxnorm p <= 0.5
p `inside` Circle _ = distance p <= 0.5
p `inside` Polygon _ (a:as) = insidePoly p a as
-- p `inside` Transform t s = transform p t `inside` s
p `inside` Transform ts s = transform p ts `inside` s

insidePoly :: Point -> Point -> [Point] -> Bool
insidePoly p a (b:c:points) = p `inTriangle` (a, b, c) || insidePoly p a (c:points)
insidePoly _ _ (_:[]) = False

getColour :: Shape -> Colour
getColour (Square c) = c
getColour (Circle c) = c
getColour (Polygon c _) = c
getColour (Transform _ s) = getColour s

colourAt :: Drawing -> Point -> Colour
colourAt (Shape s) p | p `inside` s = getColour s
                     | otherwise    = black

colourAt (Over s d) p | p `inside` s = getColour s
                      | otherwise    = colourAt d p 



getSize :: Shape -> (Double, Double)
getSize (Square _) = (0.5, 0.5)
getSize (Circle _) = (0.5, 0.5)
getSize (Polygon _ _) = (0.0, 0.0) -- TODO implement

getBounds :: Shape -> (Point, Point, Point, Point)
getBounds (Square _) = (point (-0.5) (-0.5), point 0.5 (-0.5), point 0.5 0.5, point (-0.5) 0.5)
getBounds (Circle _) = (point (-0.5) (-0.5), point 0.5 (-0.5), point 0.5 0.5, point (-0.5) 0.5)
getBounds (Transform ts s) = transformBounds (getBounds s) ts
getBounds (Polygon _ ps) =
  let
    p1 = Vector x1 y1
    p2 = Vector x2 y1
    p3 = Vector x2 y1
    p4 = Vector x1 y2
  in (p1, p2, p3, p4)
  where
    x1 = leftBound ps
    x2 = rightBound ps
    y1 = bottomBound ps
    y2 = topBound ps




transformPoints :: [Point] -> [Transformation] -> [Point]
transformPoints (p:ps) ts = ((transform p ts) : transformPoints ps ts)
transformPoints [] _ = []

transformBounds :: (Point, Point, Point, Point) -> [Transformation] -> (Point, Point, Point, Point)
transformBounds (a, b, c, d) ts = (a', b', c', d')
  where 
    a' = transform a ts
    b' = transform b ts
    c' = transform c ts
    d' = transform d ts

widthD :: Drawing -> Double
widthD (Shape s)   = widthS s
widthD (Left s d)  = widthS s + widthD d
widthD (Right s d) = widthS s + widthD d
widthD d = 
  if wS >= wD then wS
  else wD
  where
    wS = widthS (shape d)
    wD = widthD (drawing d)

widthS :: Shape -> Double
widthS s = 
  if abs (x1 - x2) >= abs (x2 - x3) then abs (x1 - x2)
  else abs (x2 - x3)
  where
    (p1, p2, p3, _) = getBounds s
    (Vector x1 _) = p1
    (Vector x2 _) = p2
    (Vector x3 _) = p3

-- Gets the height of the 
heightD :: Drawing -> Double
heightD (Shape s)   = heightS s
heightD (Above s d) = heightS s + heightD d
heightD (Below s d) = heightS s + heightD d
heightD d = 
  if hS >= hD then hS
  else hD
  where
    hS = heightS (shape d)
    hD = heightD (drawing d)

heightS :: Shape -> Double
heightS s = 
  if abs (y1 - y2) >= abs (y2 - y3) then abs (y1 - y2)
  else abs (y2 - y3)
  where
    (p1, p2, p3, _) = getBounds s
    (Vector y1 _) = p1
    (Vector y2 _) = p2
    (Vector y3 _) = p3

leftBound, rightBound, topBound, bottomBound :: [Point] -> Double
leftBound (p:ps) = if x1 <= x2 then x1 else x2
  where
    (Vector x1 _) = p
    x2 = leftBound ps
leftBound [] = 1/0

rightBound (p:ps) = if x1 > x2 then x1 else x2
  where
    (Vector x1 _) = p
    x2 = rightBound ps
rightBound [] = -1/0

topBound (p:ps) = if y1 > y2 then y1 else y2
  where
    (Vector _ y1) = p
    y2 = topBound ps
topBound [] = -1/0

bottomBound (p:ps) = if y1 <= y2 then y1 else y2
  where
    (Vector _ y1) = p
    y2 = bottomBound ps
bottomBound [] = 1/0