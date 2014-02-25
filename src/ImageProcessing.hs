module ImageProcessing where

import Codec.Picture (
    Image,Pixel,Pixel8,
    imageWidth,imageHeight,pixelAt,
    pixelMap,generateImage)
import Codec.Picture.Types (Pixel32)

import Data.Vector (Vector)
import qualified Data.Vector as Vector (map,enumFromStepN)

valueInPoint :: (Integral a,Pixel a,Num b) => Int -> Int -> Image a -> b
valueInPoint x y image
    | x < 0 || x >= imageWidth image || y < 0 || y > imageHeight image = 0
    | otherwise = fromIntegral (pixelAt image x y)

horizontalLine :: (Integral a,Pixel a,Num b) => Int -> Int -> Int -> Image a -> Vector b
horizontalLine fromx fromy tox image =
    Vector.map (\x -> valueInPoint x fromy image)
        (Vector.enumFromStepN fromx step n) where
            step = signum (tox - fromx)
            n = abs (tox - fromx + 1)

verticalLine :: (Integral a,Pixel a,Num b) => Int -> Int -> Int -> Image a -> Vector b
verticalLine fromx fromy toy image =
    Vector.map (\y -> valueInPoint fromx y image)
        (Vector.enumFromStepN fromy step n) where
            step = signum (toy - fromy)
            n = abs (toy - fromy + 1)

addImage :: Maybe (Image Pixel32) -> Image Pixel8 -> Maybe (Image Pixel32)
addImage Nothing image = Just (pixelMap fromIntegral image)
addImage (Just accuImage) image = Just (generateImage generatingFunction width height) where
    width = imageWidth accuImage
    height = imageHeight accuImage
    generatingFunction x y = pixelAt accuImage x y + fromIntegral (pixelAt image x y)

finalizeAverageImage :: (Maybe (Image Pixel32)) -> Int -> Maybe (Image Pixel8)
finalizeAverageImage Nothing _ = Nothing
finalizeAverageImage (Just image) n
    | n <= 0 = Nothing
    | otherwise = Just (pixelMap (\p -> fromIntegral (p `div` fromIntegral n)) image)

