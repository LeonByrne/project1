-- module Main (main) where

-- import Lib

-- main :: IO ()
-- main = someFunc

module Main where

import Lib    (render, defaultWindow)
import Shapes (square, circle, white, scale, scale1, scaleX, translate, blank, over)

example = (square `scale1` 0.9 `translate` (0.05, 0.0)) `over` ((circle `scale1` 1) `over` blank)

main = render "Output.png" defaultWindow example