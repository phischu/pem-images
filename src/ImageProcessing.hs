{-# LANGUAGE FlexibleInstances,TypeFamilies #-}
module ImageProcessing where

import Codec.Picture (
    Image,Pixel,Pixel8,
    imageWidth,imageHeight,pixelAt,
    pixelMap,generateImage)
import Codec.Picture.Types (
    Pixel32,
    pixelFold,
    createMutableImage,writePixel,freezeImage)

import Data.List (
    foldl')
import qualified Data.IntMap.Strict as IntMap (
    empty,elems,alter,delete)
import Data.Array (
    Array,assocs)
import Data.Array.ST (
    STArray,runSTArray,newArray,newArray_,readArray,writeArray)
import Control.Monad.ST (
    ST,runST)
import Data.STRef.Strict (
    newSTRef,readSTRef,writeSTRef,modifySTRef)
import qualified Data.UnionFind.ST as UnionFind (
    Point,fresh,equivalent,union,descriptor)

import Control.Monad (when)
import Data.Traversable (forM)

import Data.Vector (Vector)
import qualified Data.Vector as Vector (map,enumFromStepN,length)

valueInPoint :: (Integral a,Pixel a,Num b) => Int -> Int -> Image a -> b
valueInPoint x y image
    | x < 0 || x >= imageWidth image || y < 0 || y > imageHeight image = 0
    | otherwise = fromIntegral (pixelAt image x y)

averageAroundPoint :: (Integral a,Pixel a,Num b,Fractional b) => Int -> Int -> Int -> Image a -> b
averageAroundPoint x y r image = sum pixelvalues / fromIntegral (length pixelvalues) where
    pixelvalues = do
        dx <- [-r .. r]
        dy <- [-r .. r]
        return (valueInPoint (x+dx) (y+dy) image)

averageOfImage :: (Pixel a,Integral a,Num b,Fractional b) => Image a -> b
averageOfImage image = sumOfPixels / numberOfPixels where
    sumOfPixels = pixelFold addPixel 0 image where
    addPixel accumulator _ _ pixelvalue = accumulator + fromIntegral pixelvalue
    numberOfPixels = fromIntegral (imageWidth image * imageHeight image)

numberOfIslands :: Image Pixel8 -> Double
numberOfIslands image = fromIntegral (length (connectedComponents image))

averageAreaOfIslands :: Image Pixel8 -> Double
averageAreaOfIslands image = if numberofislands == 0.0 then 0.0 else numberofislandpixels / numberofislands where
    numberofislands = numberOfIslands image
    numberofislandpixels = pixelFold countpixel 0 image
    countpixel accumulator _ _ pixelvalue
        | pixelvalue == 0 = accumulator
        | otherwise = accumulator + 1

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

toLineImages :: [Vector (Vector Pixel8)] -> Vector (Image Pixel8)
toLineImages = Vector.map accumulateImage . sequence

accumulateImage :: [Vector Pixel8] -> Image Pixel8
accumulateImage imagelines = runST (do
    let width = length imagelines
        height = case imagelines of
            [] -> 0
            (imageline:_) -> Vector.length imageline
    image <- createMutableImage width height 0
    xref <- newSTRef 0
    forM imagelines (\imageline -> do
        yref <- newSTRef 0
        forM imageline (\pixelvalue -> do
            x <- readSTRef xref
            y <- readSTRef yref
            writePixel image x y pixelvalue
            modifySTRef yref (+1))
        modifySTRef xref (+1))
    freezeImage image)

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

accumulateComponents :: Array (Int,Int) Int -> [[(Int,Int)]]
accumulateComponents labelarray = IntMap.elems (IntMap.delete 0 (foldl' insertPosition IntMap.empty (assocs labelarray))) where
    insertPosition accumulator (position,label) =
        IntMap.alter (maybe (Just [position]) (\positions -> Just (position:positions))) label accumulator

