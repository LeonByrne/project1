
{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Lib    (render)
import Shapes (fill, square, circle, green, red, white, scale, scale1, scaleX, scaleY, shearX, shearY, shear1, translate, blank, over)
import Server

import Web.Scotty

example = (square `scale1` 0.5 `shearY` 0.5 `fill` red) `over` ((circle `scale1` 0.9 `fill` green) `over` blank)
-- example = ((circle `scale1` 0.5 `scaleX` 2 `translate` (0, 0.25) `fill` green) `over` blank)
-- example = ((scale1 (circle `scale1` 0.1 `translate` (0, 0.25) `scaleY` 10 `fill` green) 0.1) `over` blank)

-- main = render "Output.png" example

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do 
    text "hello"

  get "/circle" $ do
    let imgData = example1
    setHeader "Content-Type" "image/png"
    raw imgData
    
  get "/diamond" $ do
    let imgData = example2
    setHeader "Content-Type" "image/png"
    raw imgData

  get "/eye" $ do
    let imgData = example3
    setHeader "Content-Type" "image/png"
    raw imgData

  get "/triangle" $ do
    let imgData = example4
    setHeader "Content-Type" "image/png"
    raw imgData

  get "/above" $ do
    let imgData = example5
    setHeader "Content-Type" "image/png"
    raw imgData
