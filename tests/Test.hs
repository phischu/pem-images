module Main where

import ImageLoading (loadImage)
import ImageProcessing (numberOfIslands,juicyToImage,binarize)

import Control.Error (runEitherT)

main :: IO ()
main = do
    Right image <- runEitherT (loadImage "tests/ConnectedComponents.bmp")
    print (numberOfIslands (binarize 20 (juicyToImage image)))
