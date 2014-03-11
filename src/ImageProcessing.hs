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

import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set (union,singleton)
import qualified Data.IntMap.Strict as IntMap (empty,elems,insertWith,delete)
import Data.Array (Array,assocs)
import Data.Array.ST (STArray,runSTArray,newArray,newArray_,readArray,writeArray)
import Control.Monad.ST (ST)
import Data.STRef.Strict (newSTRef,modifySTRef,readSTRef,writeSTRef)
import qualified Data.UnionFind.ST as UnionFind (Point,fresh,equivalent,union,descriptor)

import Control.Monad (forM,when)

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

connectedComponents :: Image Pixel8 -> [Set (Int,Int)]
connectedComponents image = accumulateComponents (labelArray image)

labelArray :: Image Pixel8 -> Array (Int,Int) Int
labelArray image = runSTArray (do
    currentlabelref <- newSTRef 1
    let lastx = imageWidth image - 1
        lasty = imageHeight image - 1
    zero <- UnionFind.fresh 0
    pointimage <- newArray_ ((0,0),(lastx,lasty)) :: ST s (STArray s (Int,Int) (UnionFind.Point s Int))
    forM [0..lasty] (\y -> do
        forM [0..lastx] (\x -> do
            writeArray pointimage (x,y) zero
            when (pixelAt image x y /= 0) (do
                leftpoint <- if x <= 0 then return zero else readArray pointimage (x-1,y)
                upperpoint <- if y <= 0 then return zero else readArray pointimage (x,y-1)
                leftiszero <- UnionFind.equivalent leftpoint zero
                upperiszero <- UnionFind.equivalent upperpoint zero
                case (leftiszero,upperiszero) of
                    (True,True) -> do
                        currentlabel <- readSTRef currentlabelref
                        point <- UnionFind.fresh currentlabel
                        writeArray pointimage (x,y) point
                        writeSTRef currentlabelref (currentlabel + 1)
                    (True,False) -> do
                        writeArray pointimage (x,y) upperpoint
                    (False,True) -> do
                        writeArray pointimage (x,y) leftpoint
                    (False,False) -> do
                        writeArray pointimage (x,y) leftpoint
                        UnionFind.union leftpoint upperpoint)))
    labelimage <- newArray ((0,0),(lastx,lasty)) 0
    forM [0..lasty] (\y -> do
        forM [0..lastx] (\x -> do
            point <- readArray pointimage (x,y)
            label <- UnionFind.descriptor point
            writeArray labelimage (x,y) label))     
    return labelimage)

accumulateComponents :: Array (Int,Int) Int -> [Set (Int,Int)]
accumulateComponents labelarray = IntMap.elems (IntMap.delete 0 (foldl' insertPosition IntMap.empty (assocs labelarray))) where
    insertPosition accumulator (position,label) =
        IntMap.insertWith Set.union label (Set.singleton position) accumulator

toBoxedImage :: Image Pixel8 -> GrayImage
toBoxedImage image = makeImage (imageHeight image) (imageWidth image) (\r c -> fromIntegral (pixelAt image c r))

fromBoxedImage :: GrayImage -> Image Pixel8
fromBoxedImage image = generateImage generatingFunction width height where
    width = cols image
    height = rows image
    generatingFunction x y = floor ((ref image y x - minintensity) / intensityrange * 255.0)
    minintensity = minIntensity image
    intensityrange = maxIntensity image - minintensity
