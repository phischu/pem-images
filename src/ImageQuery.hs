module ImageQuery where

import ImageProcessing (
    Image,Rect,Threshold,
    valueInPoint,averageAroundPoint,averageOfImage,
    cutOut,binarize,applyStencil,
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
    NumberOfIslands |
    AverageAreaOfIslands |
    AverageOutlineOfIslands deriving Show

data LineQuery =
    HorizontalLine {
        fromX :: Int,
        fromY :: Int,
        toX :: Int} |
    VerticalLine {
        fromX :: Int,
        fromY :: Int,
        toY :: Int} deriving Show

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

runTableQuery :: Image Bool -> Double -> Image Word8 -> TableQuery ->  Double
runTableQuery _ _ image (ValueInPoint x y) = fromIntegral (valueInPoint x y image)
runTableQuery _ _ image (AverageAroundPoint x y r) = averageAroundPoint x y r image
runTableQuery _ _ image AverageOfImage = averageOfImage image
runTableQuery _ numberofislands _ NumberOfIslands = numberofislands
runTableQuery binaryimage numberofislands _ AverageAreaOfIslands = numberOfTruePixels binaryimage / numberofislands
runTableQuery binaryimage numberofislands _ AverageOutlineOfIslands = numberOfOutlinePixels binaryimage / numberofislands

runTableQueries :: Rect -> Image Bool -> Threshold -> Vector TableQuery -> Image Word8 -> Vector Double
runTableQueries rect stencilimage threshold tablequeries image = Vector.map (runTableQuery maskedimage numberofislands image) tablequeries where
    cutimage = cutOut rect image
    binaryimage = binarize threshold cutimage
    maskedimage = applyStencil stencilimage binaryimage
    numberofislands = fromIntegral (numberOfIslands binaryimage)


lineFold :: Vector LineQuery -> Fold (Image Word8) (Vector (Image Word8))
lineFold linequeries = fmap toLineImages (Fold.premap (runLineQueries linequeries) Fold.list)

runLineQuery :: LineQuery -> Image Word8 -> Unboxed.Vector Word8
runLineQuery (HorizontalLine fromx fromy tox) image = horizontalLine fromx fromy tox image
runLineQuery (VerticalLine fromx fromy toy) image = verticalLine fromx fromy toy image

runLineQueries :: Vector LineQuery -> Image Word8 -> Vector (Unboxed.Vector Word8)
runLineQueries linequeries image = Vector.map (flip runLineQuery image) linequeries


averageImageFold :: Bool -> Fold (Image Word8) (Maybe (Image Word8))
averageImageFold False = Fold const () (const Nothing)
averageImageFold True = finalizeAverageImage <$> sumImageFold <*> Fold.length

sumImageFold :: Fold (Image Word8) (Maybe (Image Integer))
sumImageFold = Fold addImage Nothing id
