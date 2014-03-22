module Main where

import ImageLoading (loadImage)
import ImageProcessing (
    numberOfIslands,juicyToImage,binarize,applyStencil,identityStencil)

import Control.Error (runEitherT)

main :: IO ()
main = do
    Right image <- runEitherT (loadImage "tests/ConnectedComponents.bmp")
    print (numberOfIslands (applyStencil (identityStencil 1080 1032) (binarize 20 (juicyToImage image))))
