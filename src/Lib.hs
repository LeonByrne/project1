module Lib
    ( render
    ) where

import Codec.Picture
import Shapes
import Render

render :: String -> Drawing -> IO ()
render path sh = writePng path $ render1 sh (500, 500)