module Render(
  render
) where

import Prelude hiding (Left, Right)
import Shapes
import Codec.Picture

-- Overall rendering function
render :: Drawing -> (Int, Int) -> Image PixelRGB8
render d (w, h) = generateImage pixelRenderer w h
  where 
    pixelRenderer x y = colourAt d (x', y')
      where x' = (-0.5) + fromIntegral x / fromIntegral w
            y' = (-0.5) + fromIntegral y / fromIntegral h

-- Maths functions
distance :: Point -> Double
distance (x, y) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (x, y) = max (abs x) (abs y)

cross :: Point -> Point -> Double
cross (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

mult :: Matrix -> Point -> Point
mult (Matrix a b c d) v =  ((cross (a, b) v), (cross (c, d) v))

invert :: Matrix -> Matrix
invert (Matrix a b c d) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

-- Barymetric point/triangle collision detection
inTriangle :: Point -> (Point, Point, Point) -> Bool
inTriangle p (a, b, c) = 
  let denominator = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
      alpha = ((y2 - y3) * (x - x3) + (x3 - x2) * (y - y3)) / denominator
      beta  = ((y3 - y1) * (x - x3) + (x1 - x3) * (y - y3)) / denominator
      gamma = 1 - alpha - beta
  in alpha >= 0 && beta >= 0 && gamma >= 0
    where
      (x, y)   = p
      (x1, y1) = a
      (x2, y2) = b
      (x3, y3) = c

-- These perform transformations on points instead of shapes
-- Acting kinda inverse to a true transformation of a shape
-- The points inside have the transformations performed instead of the shape
-- Bad name but I have other transform functions later so I need the name
transformInv :: Point -> [Transformation] -> Point
transformInv p (t:ts) = transformInv1 (transformInv p ts) t
transformInv p [] = p

transformInv1 :: Point -> Transformation -> Point
transformInv1 (x, y) (Scale sx sy) = ((x / sx), (y / sy))  
transformInv1 (x, y) (Shear sx sy) = ((x + sx*y), (y + sy*x))
transformInv1 (x, y) (Translate tx ty) = ((x - tx), (y - ty)) 
transformInv1 p (Rotate m) = invert m `mult` p


inside :: Point -> Shape -> Bool
p `inside` Square _ = maxnorm p <= 0.5
p `inside` Circle _ = distance p <= 0.5
p `inside` Polygon _ (a:as) = insidePoly p a as
p `inside` Polygon _ [] = False -- Need to catch this
p `inside` Transform ts s = transformInv p ts `inside` s -- Again not transforming shapes but points to match instead

-- This works by splitting the polygon into triangles
-- a is the first point, b and c are the second and third
-- It will advance b and c to the next points
insidePoly :: Point -> Point -> [Point] -> Bool
insidePoly p a (b:c:points) = p `inTriangle` (a, b, c) || insidePoly p a (c:points)
insidePoly _ _ (_:[]) = False -- Can still have one left but can't do anything with it
insidePoly _ _ [] = False

-- This just extracts the colour
getColour :: Shape -> Colour
getColour (Square c) = c
getColour (Circle c) = c
getColour (Polygon c _) = c
getColour (Transform _ s) = getColour s

-- Gets the colour a given point should be, or black if none
-- found
colourAt :: Drawing -> Point -> Colour
colourAt (Shape s) p | p `inside` s = getColour s
                     | otherwise    = black

colourAt (Over s d) p | p `inside` s = getColour s
                      | otherwise    = colourAt d p 

-- These are just performing some translations before the get inside call
colourAt (Left s d) p =
  let
    (x, y) = p
    x' = x - minXD d + (width s / 2)
    p' = (x', y)
  in if p' `inside` s then getColour s else colourAt d p

colourAt (Right s d) p = 
  let
    (x, y) = p
    x' = x - maxXD d - (width s / 2)
    p' = (x', y)
  in if p' `inside` s then getColour s else colourAt d p

colourAt (Above s d) p = 
  let
    (x, y) = p
    y' = y - minYD d + (height s / 2)
    p' = (x, y')
  in if p' `inside` s then getColour s else colourAt d p

colourAt (Below s d) p = 
  let
    (x, y) = p
    y' = y - maxYD d - (height s / 2)
    p' = (x, y')
  in if p' `inside` s then getColour s else colourAt d p

-- Get a bounding box for a given shape
getBounds :: Shape -> (Point, Point, Point, Point)
getBounds (Square _) = (((-0.5), (-0.5)), (0.5, (-0.5)), (0.5, 0.5), ((-0.5), 0.5))
getBounds (Circle _) = (((-0.5), (-0.5)), (0.5, (-0.5)), (0.5, 0.5), ((-0.5), 0.5))
getBounds (Transform ts s) = 
  let 
    p1 = (x1, y1)
    p2 = (x2, y1)
    p3 = (x2, y1)
    p4 = (x1, y2)
  in (p1, p2, p3, p4) -- top left, top right, bottom right, bottom right
  where
    (a, b, c, d) = transformBounds (getBounds s) ts
    x1 = minXP [a, b, c, d]
    x2 = maxXP [a, b, c, d]
    y1 = minYP [a, b, c, d]
    y2 = maxYP [a, b, c, d]

getBounds (Polygon _ ps) =
  let
    p1 = (x1, y1)
    p2 = (x2, y1)
    p3 = (x2, y1)
    p4 = (x1, y2)
  in (p1, p2, p3, p4) -- top left, top right, bottom right, bottom right
  where
    x1 = minXP ps
    x2 = maxXP ps
    y1 = minYP ps
    y2 = maxYP ps

-- Apply all transformatios to a bounding box
transformBounds :: (Point, Point, Point, Point) -> [Transformation] -> (Point, Point, Point, Point)
transformBounds (a, b, c, d) ts = (a', b', c', d')
  where 
    a' = transform a ts
    b' = transform b ts
    c' = transform c ts
    d' = transform d ts

transform :: Point -> [Transformation] -> Point
transform p (t:ts) = transform1 (transform p ts) t
transform p [] = p

transform1 :: Point -> Transformation -> Point
transform1 (x, y) (Scale sx sy) = ((x * sx), (y * sy))  
transform1 (x, y) (Shear sx sy) = ((x - sx*y), (y - sy*x))
transform1 (x, y) (Translate tx ty) = ((x + tx), (y + ty)) 
transform1 p (Rotate m) = m `mult` p

width :: Shape -> Double
width s = 
  if abs (x1 - x2) >= abs (x2 - x3) then abs (x1 - x2)
  else abs (x2 - x3)
  where
    (p1, p2, p3, _) = getBounds s
    (x1, _) = p1
    (x2, _) = p2
    (x3, _) = p3

height :: Shape -> Double
height s = 
  if abs (y1 - y2) >= abs (y2 - y3) then abs (y1 - y2)
  else abs (y2 - y3)
  where
    (p1, p2, p3, _) = getBounds s
    (_, y1) = p1
    (_, y2) = p2
    (_, y3) = p3

-- Thes functions get the min and max extents for drawings
minXD, maxXD, minYD, maxYD :: Drawing -> Double
minXD (Shape s) = minXS s
minXD (Left s d) = if minD <= minT then minD else minT
  where
    minD = minXD d
    minT = minD - (width s / 2) + minXS s
minXD (Right s d) = if minD <= minT then minD else minT
  where
    minD = minXD d
    minT = minD + (width s / 2) + minXS s
minXD d = if minS <= minD then minS else minD
  where
    minS = minXS (shape d)
    minD = minXD (drawing d)

maxXD (Shape s) = maxXS s
maxXD (Left s d) = if maxD > maxS then maxD else maxS
  where
    maxD = maxXD d
    maxS = maxD - (width s / 2) + maxXS s
maxXD (Right s d) = if maxD <= maxS then maxD else maxS
  where
    maxD = maxXD d
    maxS = maxD + (width s / 2) + maxXS s
maxXD d = if maxS <= maxD then maxS else maxD
  where
    maxS = maxXS (shape d)
    maxD = maxXD (drawing d)

minYD (Shape s) = minYS s
minYD (Above s d) = if minD < minS then minD else minS
  where
    minD = minYD d
    minS = minD - (height s / 2) + minYS s
minYD (Below s d) = if minD < minS then minD else minS
  where
    minD = minYD d
    minS = minD + (height s / 2) + minYS s
minYD d = if minD < minS then minD else minS
  where
    minD = minYD (drawing d)
    minS = minYS (shape d)

maxYD (Shape s) = maxYS s
maxYD (Above s d) = if maxD > maxS then maxD else maxS
  where
    maxD = maxYD d
    maxS = maxD - (height s / 2) + maxYS s
maxYD (Below s d) = if maxD > maxS then maxD else maxS
  where
    maxD = maxYD d
    maxS = maxD + (height s / 2) + maxYS s
maxYD d = if maxD > maxS then maxD else maxS
  where
    maxD = maxYD (drawing d)
    maxS = maxYS (shape d)


-- These functions get the min and max bounds for shapes
minXS, maxXS, maxYS, minYS :: Shape -> Double
minXS s = minXP bounds
  where 
    (a, b, c, d) = getBounds s
    bounds = [a, b, c, d]
maxXS s = maxXP bounds
  where 
    (a, b, c, d) = getBounds s
    bounds = [a, b, c, d]
maxYS s = maxYP bounds
  where 
    (a, b, c, d) = getBounds s
    bounds = [a, b, c, d]
minYS s = minYP bounds
  where 
    (a, b, c, d) = getBounds s
    bounds = [a, b, c, d]

-- These functions get the min and max bounds for lists of points
minXP, maxXP, maxYP, minYP :: [Point] -> Double
minXP (p:ps) = if x1 <= x2 then x1 else x2
  where
    (x1, _) = p
    x2 = minXP ps
minXP [] = 1/0

maxXP (p:ps) = if x1 > x2 then x1 else x2
  where
    (x1, _) = p
    x2 = maxXP ps
maxXP [] = -1/0

maxYP (p:ps) = if y1 > y2 then y1 else y2
  where
    (_, y1) = p
    y2 = maxYP ps
maxYP [] = -1/0

minYP (p:ps) = if y1 <= y2 then y1 else y2
  where
    (_, y1) = p
    y2 = minYP ps
minYP [] = 1/0
