module Main where

import ImageLoading (loadImage)
import ImageProcessing (numberOfIslands,fromBoxedImage,toBoxedImage)

import Data.Image.Binary (toBinaryImage)
import Data.Image.Boxed (label)

import Codec.Picture (writeBitmap)

import Control.Error (runEitherT)

main :: IO ()
main = do
    Right image <- runEitherT (loadImage "tests/ConnectedComponents.bmp")
    print (numberOfIslands image)
