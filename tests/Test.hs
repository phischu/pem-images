module Main where

import ImageLoading (loadImage)
import ImageQuery (prepareImage,initialImageQueryParameters)
import ImageProcessing (numberOfIslands,juicyToImage)

import Control.Error (runEitherT)

main :: IO ()
main = do
    Right image <- runEitherT (loadImage "tests/ConnectedComponents.bmp")
    print (numberOfIslands (snd (prepareImage initialImageQueryParameters (juicyToImage image))))
