{-# LANGUAGE FlexibleInstances,TypeFamilies #-}
module ImageProcessing where

import Codec.Picture (
    Image,Pixel,Pixel8,
    imageWidth,imageHeight,pixelAt,
    pixelMap,generateImage)
import Codec.Picture.Types (Pixel32)

import Data.Image (GrayImage,makeImage)
import Data.Image.Binary (toBinaryImage)
import Data.Image.Internal (maxIntensity,minIntensity,cols,rows,ref)
import Data.Image.Boxed (label)

import Data.Graph (graphFromEdges,dff)
import Data.Tree (flatten)

import Control.Monad (guard)

import Data.Vector (Vector)
import qualified Data.Vector as Vector (map,enumFromStepN)

valueInPoint :: (Integral a,Pixel a,Num b) => Int -> Int -> Image a -> b
valueInPoint x y image
    | x < 0 || x >= imageWidth image || y < 0 || y > imageHeight image = 0
    | otherwise = fromIntegral (pixelAt image x y)

numberOfIslands :: Image Pixel8 -> Double
numberOfIslands image = fromIntegral (length (connectedComponents image))

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

connectedComponents :: Image Pixel8 -> [[(Int,Int)]]
connectedComponents image = map (map vertexToPosition . flatten) (dff imagegraph) where
    vertexToPosition vertex = let (position,_,_) = vertexInfo vertex in position
    (imagegraph,vertexInfo,_) = graphFromEdges [
        ((x,y),(x,y),[(x+1,y),(x-1,y),(x,y+1),(x,y-1)]) |
            x <- [0..imageWidth image - 1],
            y <- [0..imageHeight image - 1]]
    {-graphFromEdges (do
        x <- [0..imageWidth image - 1]
        y <- [0..imageHeight image - 1]
        guard (pixelAt image x y /= 0)
        let adjacentpositions = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
        return ((x,y),(x,y),adjacentpositions))-}

toBoxedImage :: Image Pixel8 -> GrayImage
toBoxedImage image = makeImage (imageHeight image) (imageWidth image) (\r c -> fromIntegral (pixelAt image c r))

fromBoxedImage :: GrayImage -> Image Pixel8
fromBoxedImage image = generateImage generatingFunction width height where
    width = cols image
    height = rows image
    generatingFunction x y = floor ((ref image y x - minintensity) / intensityrange * 255.0)
    minintensity = minIntensity image
    intensityrange = maxIntensity image - minintensity
