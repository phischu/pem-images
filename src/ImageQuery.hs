module ImageQuery where

import ImageProcessing (
    Threshold,
    valueInPoint,averageAroundPoint,averageOfImage,
    numberOfIslands,numberOfNonZeroPixels,numberOfOutlinePixels,
    horizontalLine,verticalLine,toLineImages,
    addImage,finalizeAverageImage,)

import Codec.Picture (Image,Pixel8)
import Codec.Picture.Types (Pixel32)

import Control.Foldl (Fold(Fold))
import qualified Control.Foldl as Fold (list,premap,length)
import Control.Applicative ((<$>),(<*>))

import Data.Vector (Vector)
import Data.Vector as Vector (map)

data ImageQuery = ImageQuery {
    binarizationThreshold :: Pixel8,
    tableQueries :: Vector TableQuery,
    lineQueries :: Vector LineQuery,
    averageImageQuery :: Bool} deriving Show
    
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
    lineImages :: Vector (Image Pixel8),
    averageImage :: Maybe (Image Pixel8)}

runImageQuery :: ImageQuery -> Fold (Image Pixel8) ImageQueryResult
runImageQuery imagequery =
    ImageQueryResult <$>
    tableFold (binarizationThreshold imagequery) (tableQueries imagequery) <*>
    lineFold (lineQueries imagequery) <*>
    averageImageFold (averageImageQuery imagequery)


tableFold :: Threshold ->  Vector TableQuery -> Fold (Image Pixel8) [Vector Double]
tableFold threshold tablequeries = Fold.premap (runTableQueries threshold tablequeries) Fold.list

runTableQuery :: Double -> TableQuery -> Image Pixel8 -> Double
runTableQuery _ (ValueInPoint x y) image = valueInPoint x y image
runTableQuery _ (AverageAroundPoint x y r) image = averageAroundPoint x y r image
runTableQuery _ AverageOfImage image = averageOfImage image
runTableQuery numberofislands NumberOfIslands _ = numberofislands
runTableQuery numberofislands AverageAreaOfIslands  image = numberOfNonZeroPixels image / numberofislands
runTableQuery numberofislands AverageOutlineOfIslands image = numberOfOutlinePixels image / numberofislands

runTableQueries :: Threshold -> Vector TableQuery -> Image Pixel8 -> Vector Double
runTableQueries threshold tablequeries image = Vector.map (flip (runTableQuery numberofislands) image) tablequeries where
    numberofislands = numberOfIslands threshold image


lineFold :: Vector LineQuery -> Fold (Image Pixel8) (Vector (Image Pixel8))
lineFold linequeries = fmap toLineImages (Fold.premap (runLineQueries linequeries) Fold.list)

runLineQuery :: LineQuery -> Image Pixel8 -> Vector Pixel8
runLineQuery (HorizontalLine fromx fromy tox) image = horizontalLine fromx fromy tox image
runLineQuery (VerticalLine fromx fromy toy) image = verticalLine fromx fromy toy image

runLineQueries :: Vector LineQuery -> Image Pixel8 -> Vector (Vector Pixel8)
runLineQueries linequeries image = Vector.map (flip runLineQuery image) linequeries


averageImageFold :: Bool -> Fold (Image Pixel8) (Maybe (Image Pixel8))
averageImageFold False = Fold const () (const Nothing)
averageImageFold True = finalizeAverageImage <$> sumImageFold <*> Fold.length

sumImageFold :: Fold (Image Pixel8) (Maybe (Image Pixel32))
sumImageFold = Fold addImage Nothing id
