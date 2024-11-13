module Render(
  render1
) where

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

transform :: Point -> Transformation -> Point
transform (Vector x y) (Scale sx sy) = Vector (x / sx) (y / sy)  
transform (Vector x y) (Shear sx sy) = Vector (x + sx*y) (y + sy*x)
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
colourAt (Shape s) p | p `inside` s = getColour s
                     | otherwise    = black

colourAt (Over s d) p | p `inside` s = getColour s
                      | otherwise    = colourAt d p 