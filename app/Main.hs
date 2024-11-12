-- module Main (main) where

-- import Lib

-- main :: IO ()
-- main = someFunc

module Main where

import Lib    (render, defaultWindow)
import Shapes (circle, white, scale, scale1, translate, blank, over)

example = (circle `scale1` 1) `over` blank

main = render "Output.png" defaultWindow example