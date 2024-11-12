module Lib
    ( Window, defaultWindow, someFunc, render
    ) where

import Codec.Picture
import Shapes

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Window = Window Point Point (Int,Int)

-- Default window renders a small region around the origin into
-- a 100x100 pixel image
-- Note the comment below on the 'render' function. If you increase
-- the size of the output image to around 500 by 500 you'll see what
-- I mean by inefficient
defaultWindow :: Window
defaultWindow = Window (point (-1.5) (-1.5)) (point 1.5 1.5) (500,500)


-- Generate a list of evenly spaced samples between two values.
-- e.g. samples -1.5 +1.5 25 generates 25 samples evenly spaced
--      between the two bounds: [ -1.5, -1.375, -1.25 ... ]
samples :: Double -> Double -> Int -> [Double]
samples c0 c1 n = take n [ c0, c0 + (c1-c0) / fromIntegral (n-1) .. ]


-- Generate the matrix of points corresponding to the pixels of a window.
pixels :: Window -> [[Point]]
pixels (Window p0 p1 (w,h)) =
  [ [ point x y | x <- samples (getX p0) (getX p1) w ]
                | y <- reverse $ samples (getY p0) (getY p1) h
  ]

-- generate list of all screen coordinates in window
coords :: Window -> [[(Int,Int)]]
coords (Window _ _ (w,h)) = [ [(x,y) | x <- [0..w]] | y <- [0..h] ]


render :: String -> Window -> Drawing -> IO ()
render path win sh = writePng path $ generateImage pixRenderer w h
    where
      Window _ _ (w,h) = win

      pixRenderer x y = PixelRGB8 c c c where c = colorForImage $ mapPoint win (x,y)

      -- This now runs in O(1) time, lookup is ignored
      --   It makes only the point required
      --   Looked at how pixels and samples worked. Had to change the 
      --   calculation of y' for some reason to get it to make the old mapPoint,
      --   the resulting images were flipped vertically otherwise.
      mapPoint :: Window -> (Int,Int) -> Point
      mapPoint (Window p0 p1 (w, h)) (x, y) = point x' y'
        where
          Vector minX minY = p0
          Vector maxX maxY = p1
          x' = minX + ((maxX - minX) * fromIntegral x) / fromIntegral (w-1)
          y' = maxY - ((maxY - minY) * fromIntegral y) / fromIntegral (h-1)

      lookup1 :: (Int,Int) -> [((Int,Int), Point)] -> Point
      lookup1 a m = case lookup a m of
                      Nothing -> point 0 0
                      Just x  -> x

      locations :: [ ((Int,Int), Point) ]
      locations = concat $ zipWith zip (coords win) (pixels win)
      colorForImage p | colourAt sh p == (255, 255, 255) = 255
                      | otherwise                        = 0