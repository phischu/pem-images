module ImageProcessing where

import Codec.Picture

mayPixelAt :: (Pixel a) => Image a -> Int -> Int -> Maybe a
mayPixelAt image x y
    | x < 0 || x >= imageWidth image || y < 0 || y > imageHeight image = Nothing
    | otherwise = Just (pixelAt image x y)


