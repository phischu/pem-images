module Main where

import ImageLoading (loadImage)
import ImageQuery (prepareImage,initialImageQueryParameters)
import ImageProcessing (numberOfIslands,juicyToImage,imageToJuicy,blackAndWhite)

import Codec.Picture (writeBitmap)

import Control.Error (runEitherT)

main :: IO ()
main = do
    Right image <- runEitherT (loadImage "tests/ConnectedComponentsBig.bmp")
    let (_,islandimage) = prepareImage initialImageQueryParameters (juicyToImage image)
    writeBitmap "tests/islandimage.bmp" (imageToJuicy (blackAndWhite islandimage))
    print (numberOfIslands islandimage)
