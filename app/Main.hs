
{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Server

import Web.Scotty
import Text.Blaze.Html.Renderer.Text (renderHtml)

main :: IO ()
main = scotty 3000 $ do
  get "/" $ html $ renderHtml mainPage

  get "/circle" $ do
    let imgData = circleExample
    setHeader "Content-Type" "image/png"
    raw imgData

  get "/square" $ do
    let imgData = squareExample
    setHeader "Content-Type" "image/png"
    raw imgData

  get "/rectangle" $ do
    let imgData = rectExample
    setHeader "Content-Type" "image/png"
    raw imgData

  get "/ellipse" $ do
    let imgData = ellipseExample
    setHeader "Content-Type" "image/png"
    raw imgData

  get "/polygon" $ do
    let imgData = polyExample
    setHeader "Content-Type" "image/png"
    raw imgData
    
  get "/diamond" $ do
    let imgData = example2
    setHeader "Content-Type" "image/png"
    raw imgData

  get "/eye" $ do
    let imgData = eyeExample
    setHeader "Content-Type" "image/png"
    raw imgData

  get "/snowman" $ do
    let imgData = snowmanExample
    setHeader "Content-Type" "image/png"
    raw imgData

  get "/shear" $ do
    let imgData = shearExample
    setHeader "Content-Type" "image/png"
    raw imgData

  get "/rotate" $ do
    let imgData = rotateExample
    setHeader "Content-Type" "image/png"
    raw imgData
