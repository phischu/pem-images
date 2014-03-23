module ImageQuery where

import ImageProcessing (
    Image,Rect,Threshold,
    valueInPoint,averageAroundPoint,averageOfImage,
    cutOut,binarize,applyStencil,invert,
    numberOfIslands,numberOfTruePixels,numberOfOutlinePixels,
    horizontalLine,verticalLine,toLineImages,
    addImage,finalizeAverageImage)

import Data.Word (Word8)

import Control.Foldl (Fold(Fold))
import qualified Control.Foldl as Fold (list,premap,length)
import Control.Applicative ((<$>),(<*>))

import Data.Vector (Vector)
import Data.Vector as Vector (map)
import qualified Data.Vector.Unboxed as Unboxed (Vector)

data ImageQuery = ImageQuery {
    cutRect :: Rect,
    stencilImage :: Image Bool,
    binarizationThreshold :: Word8,
    tableQueries :: Vector TableQuery,
    lineQueries :: Vector LineQuery,
    averageImageQuery :: Bool}
    
data TableQuery =
    ValueInPoint Int Int |
    AverageAroundPoint Int Int Int |
    AverageOfImage |
    IslandQuery Polarity IslandQuery deriving Show

data Polarity =
    Dark |
    Bright deriving Show

data IslandQuery =
    NumberOfIslands |
    AverageAreaOfIslands |
    AverageOutlineOfIslands deriving Show

data LineQuery =
    HorizontalLine {
        fromX :: Int,
        fromY :: Int,
        pixelsOnLine :: Int} |
    VerticalLine {
        fromX :: Int,
        fromY :: Int,
        pixelsOnLine :: Int} deriving Show

data ImageQueryResult = ImageQueryResult {
    tableRows :: [Vector Double],
    lineImages :: Vector (Image Word8),
    averageImage :: Maybe (Image Word8)}

runImageQuery :: ImageQuery -> Fold (Image Word8) ImageQueryResult
runImageQuery imagequery =
    ImageQueryResult <$>
    tableFold (cutRect imagequery) (stencilImage imagequery) (binarizationThreshold imagequery) (tableQueries imagequery) <*>
    lineFold (lineQueries imagequery) <*>
    averageImageFold (averageImageQuery imagequery)


tableFold :: Rect -> Image Bool -> Threshold ->  Vector TableQuery -> Fold (Image Word8) [Vector Double]
tableFold rect stencilimage threshold tablequeries = Fold.premap (runTableQueries rect stencilimage threshold tablequeries) Fold.list

runTableQuery :: Image Bool -> Image Bool -> Double -> Double -> Image Word8 -> TableQuery ->  Double
runTableQuery _ _ _ _ image (ValueInPoint x y) =
    fromIntegral (valueInPoint x y image)
runTableQuery _ _ _ _ image (AverageAroundPoint x y r) =
    averageAroundPoint x y r image
runTableQuery _ _ _ _ image AverageOfImage =
    averageOfImage image
runTableQuery darkislandimage _ numberofdarkislands _ _ (IslandQuery Dark islandquery) =
    runIslandQuery darkislandimage numberofdarkislands islandquery
runTableQuery _ brightislandimage _ numberofbrightislands _ (IslandQuery Bright islandquery) =
    runIslandQuery brightislandimage numberofbrightislands islandquery

runIslandQuery :: Image Bool -> Double -> IslandQuery -> Double
runIslandQuery _ numberofislands NumberOfIslands = numberofislands
runIslandQuery islandimage numberofislands AverageAreaOfIslands = numberOfTruePixels islandimage / numberofislands
runIslandQuery islandimage numberofislands AverageOutlineOfIslands = numberOfOutlinePixels islandimage / numberofislands

runTableQueries :: Rect -> Image Bool -> Threshold -> Vector TableQuery -> Image Word8 -> Vector Double
runTableQueries rect stencilimage threshold tablequeries image =
    Vector.map (runTableQuery darkislandimage brightislandimage numberofdarkislands numberofbrightislands image) tablequeries where
        cutimage = cutOut rect image
        binaryimage = binarize threshold cutimage
        brightislandimage = applyStencil stencilimage binaryimage
        darkislandimage = applyStencil stencilimage (invert binaryimage)
        numberofdarkislands = fromIntegral (numberOfIslands darkislandimage)
        numberofbrightislands = fromIntegral (numberOfIslands brightislandimage)


lineFold :: Vector LineQuery -> Fold (Image Word8) (Vector (Image Word8))
lineFold linequeries = fmap toLineImages (Fold.premap (runLineQueries linequeries) Fold.list)

runLineQuery :: LineQuery -> Image Word8 -> Unboxed.Vector Word8
runLineQuery (HorizontalLine fromx fromy pixelsonline) image = horizontalLine fromx fromy pixelsonline image
runLineQuery (VerticalLine fromx fromy pixelsonline) image = verticalLine fromx fromy pixelsonline image

runLineQueries :: Vector LineQuery -> Image Word8 -> Vector (Unboxed.Vector Word8)
runLineQueries linequeries image = Vector.map (flip runLineQuery image) linequeries


averageImageFold :: Bool -> Fold (Image Word8) (Maybe (Image Word8))
averageImageFold False = Fold const () (const Nothing)
averageImageFold True = finalizeAverageImage <$> sumImageFold <*> Fold.length

sumImageFold :: Fold (Image Word8) (Maybe (Image Integer))
sumImageFold = Fold addImage Nothing id
