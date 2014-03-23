module Main where

import ImageLoading (loadImage)
import ImageProcessing (
    numberOfIslands,juicyToImage,binarize,applyStencil,identityStencil,cutOut,invert)

import Control.Error (runEitherT)

main :: IO ()
main = do
    Right image <- runEitherT (loadImage "tests/ConnectedComponents.bmp")
    print (numberOfIslands (applyStencil (identityStencil 1080 1032) (invert (binarize 20 (cutOut (2,3,613,412) (juicyToImage image))))))
