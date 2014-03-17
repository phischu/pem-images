module Main where

import ImageLoading (loadImage)
import ImageProcessing (numberOfIslands)

import Control.Error (runEitherT)

main :: IO ()
main = do
    Right image <- runEitherT (loadImage "tests/ConnectedComponents.bmp")
    print (numberOfIslands image)
