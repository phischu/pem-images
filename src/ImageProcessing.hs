module ImageProcessing where

import Codec.Picture

valueInPoint :: (Integral a,Pixel a) => Int -> Int -> Image a -> Double
valueInPoint x y image
    | x < 0 || x >= imageWidth image || y < 0 || y > imageHeight image = 0.0
    | otherwise = fromIntegral (pixelAt image x y)



