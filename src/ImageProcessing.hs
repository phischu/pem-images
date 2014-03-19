{-# LANGUAGE FlexibleInstances,TypeFamilies,BangPatterns #-}
module ImageProcessing where

import qualified Data.Array.Repa as Repa (
    Array,
    sumAllS,map,traverse,delay)
import Data.Array.Repa (
    Array,D,DIM2,extent,
    inShape,(:.)((:.)),Z(Z),index,
    (+^),Shape)
import qualified Data.Array.Repa.Repr.Vector as Repa (fromVector)

import qualified Codec.Picture as Juicy (Image,Pixel8)

import Data.Word (Word8)

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
import qualified Data.Vector as Vector (map,enumFromStepN,length,concat)

type Image a = Repa.Array D DIM2 a

juicyToImage :: Juicy.Image Juicy.Pixel8 -> Image Word8
juicyToImage = undefined

imageToJuicy :: Image Word8 -> Juicy.Image Juicy.Pixel8
imageToJuicy = undefined

type Threshold = Word8

valueInPoint :: (Num a) => Int -> Int -> Image a -> a
valueInPoint x y image = withDefault (extent image) 0 (index image) (Z:.y:.x)

averageAroundPoint :: (Num a,Integral a,Num b,Fractional b) => Int -> Int -> Int -> Image a -> b
averageAroundPoint x y r image = sum pixelvalues / (2 * fromIntegral r + 1)^2 where
    pixelvalues = do
        dx <- [-r..r]
        dy <- [-r..r]
        return (fromIntegral (valueInPoint (x+dx) (y+dy) image))

averageOfImage :: (Integral a) => Image a -> Double
averageOfImage image = sumOfPixels / numberOfPixels where
    sumOfPixels = Repa.sumAllS (Repa.map fromIntegral image :: Image Double)
    numberOfPixels = fromIntegral (w * h)
    Z:.h:.w = extent image

numberOfIslands :: Image Bool -> Int
numberOfIslands image = numberOfLabels (labelImage image)

binarize :: Threshold -> Image Word8 -> Image Bool
binarize threshold image = Repa.map (\pixelvalue -> pixelvalue > threshold) image

numberOfTruePixels :: Image Bool -> Double
numberOfTruePixels image = Repa.sumAllS (Repa.map boolToDouble image) where
    boolToDouble False = 0.0
    boolToDouble True = 1.0

numberOfOutlinePixels :: Image Bool -> Double
numberOfOutlinePixels image = numberOfTruePixels (Repa.traverse image id isOutline) where
    isOutline i (Z:.y:.x) = not (all (withDefault (extent image) False i) indices) where
        indices = [Z:.y:.x-1,Z:.y:.x+1,Z:.y-1:.x,Z:.y+1:.x]

withDefault :: (Shape sh) => sh -> a -> (sh -> a) -> sh -> a
withDefault shape def image position
    | inShape shape position = image position
    | otherwise = def

horizontalLine :: (Num a) => Int -> Int -> Int -> Image a -> Vector a
horizontalLine fromx fromy tox image =
    Vector.map (\x -> valueInPoint x fromy image)
        (Vector.enumFromStepN fromx step n) where
            step = signum (tox - fromx)
            n = abs (tox - fromx + 1)

verticalLine :: (Num a) => Int -> Int -> Int -> Image a -> Vector a
verticalLine fromx fromy toy image =
    Vector.map (\y -> valueInPoint fromx y image)
        (Vector.enumFromStepN fromy step n) where
            step = signum (toy - fromy)
            n = abs (toy - fromy + 1)

toLineImages :: [Vector (Vector Word8)] -> Vector (Image Word8)
toLineImages = Vector.map accumulateImage . sequence

accumulateImage :: [Vector Word8] -> Image Word8
accumulateImage imagelines = Repa.delay (Repa.fromVector (Z:.h:.w) (Vector.concat imagelines)) where
    w = length imagelines
    h = case imagelines of
        [] -> 0
        (imageline:_) -> Vector.length imageline

addImage :: Maybe (Image Integer) -> Image Word8 -> Maybe (Image Integer)
addImage Nothing image = Just (Repa.map fromIntegral image)
addImage (Just accuImage) image = Just (accuImage +^ (Repa.map fromIntegral image))

finalizeAverageImage :: (Maybe (Image Integer)) -> Int -> Maybe (Image Word8)
finalizeAverageImage Nothing _ = Nothing
finalizeAverageImage (Just image) n
    | n <= 0 = Nothing
    | otherwise = Just (Repa.map (\pixelvalue -> fromIntegral (pixelvalue `div` fromIntegral n)) image)

labelImage :: Image Bool -> Image Int
labelImage image = undefined{- runSTArray (do
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
    return labelimage)-}

numberOfLabels :: Image Int -> Int
numberOfLabels = undefined
