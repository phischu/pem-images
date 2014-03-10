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
import qualified Data.IntMap.Strict as IntMap (empty,elems,insertWith)
import Data.Array (Array,assocs)
import Data.Array.ST (runSTArray,newArray,readArray,writeArray)
import Data.STRef.Strict (newSTRef,modifySTRef,readSTRef,writeSTRef)
import qualified Data.IntDisjointSet as DisjointSet (empty,insert,union,lookup)

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
    labelimage <- newArray ((0,0),(lastx,lasty)) 0
    disjointsetref <- newSTRef DisjointSet.empty
    forM [0..lasty] (\y -> do
        forM [0..lastx] (\x -> do
            when (pixelAt image x y /= 0) (do
                leftlabel <- if x <= 0 then return 0 else readArray labelimage (x-1,y)
                upperlabel <- if y <= 0 then return 0 else readArray labelimage (x,y-1)
                case (leftlabel,upperlabel) of
                    (0,0) -> do
                        currentlabel <- readSTRef currentlabelref
                        writeArray labelimage (x,y) currentlabel
                        writeSTRef currentlabelref (currentlabel + 1)
                        modifySTRef disjointsetref (DisjointSet.insert currentlabel)
                    (0,_) -> do
                        writeArray labelimage (x,y) upperlabel
                    (_,0) -> do
                        writeArray labelimage (x,y) leftlabel
                    (_,_) -> do
                        writeArray labelimage (x,y) leftlabel
                        modifySTRef disjointsetref (DisjointSet.union leftlabel upperlabel))))
    forM [0..lasty] (\y -> do
        forM [0..lastx] (\x -> do
            currentlabel <- readArray labelimage (x,y)
            disjointset <- readSTRef disjointsetref
            case DisjointSet.lookup currentlabel disjointset of
                (Nothing,disjointset') -> do
                    writeArray labelimage (x,y) 0
                    writeSTRef disjointsetref disjointset'
                (Just representative,disjointset') -> do
                    writeArray labelimage (x,y) representative
                    writeSTRef disjointsetref disjointset'))
    return labelimage)

accumulateComponents :: Array (Int,Int) Int -> [Set (Int,Int)]
accumulateComponents labelarray = IntMap.elems (foldl' insertPosition IntMap.empty (assocs labelarray)) where
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
