module ImageQuery where

import ImageProcessing (valueInPoint,horizontalLine,verticalLine)

import Codec.Picture (Image,Pixel8,PixelF)

import Control.Foldl (Fold(Fold),list,premap)
import Control.Applicative ((<$>),(<*>))

import Data.Vector (Vector)
import Data.Vector as Vector (map)

data ImageQuery = ImageQuery {
    tableQueries :: Vector TableQuery,
    lineQuery :: Vector LineQuery,
    averageImageQuery :: Bool} deriving Show
    
data TableQuery =
    ValueInPoint Int Int deriving Show

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
    lineValues :: [Vector (Vector Pixel8)],
    averageImage :: Maybe (Image PixelF)}

runImageQuery :: ImageQuery -> Fold (Image Pixel8) ImageQueryResult
runImageQuery (ImageQuery tablequeries linequeries averageimagequery) =
    ImageQueryResult <$>
    tableFold tablequeries <*>
    lineFold linequeries <*>
    averageImageFold averageimagequery

tableFold :: Vector TableQuery -> Fold (Image Pixel8) [Vector Double]
tableFold tablequeries = premap (runTableQueries tablequeries) list

runTableQuery :: TableQuery -> Image Pixel8 -> Double
runTableQuery (ValueInPoint x y) image = valueInPoint x y image

runTableQueries :: Vector TableQuery -> Image Pixel8 -> Vector Double
runTableQueries tablequeries image = Vector.map (flip runTableQuery image) tablequeries

lineFold :: Vector LineQuery -> Fold (Image Pixel8) [Vector (Vector Pixel8)]
lineFold linequeries = premap (runLineQueries linequeries) list

runLineQuery :: LineQuery -> Image Pixel8 -> Vector Pixel8
runLineQuery (HorizontalLine fromx fromy tox) image = horizontalLine fromx fromy tox image
runLineQuery (VerticalLine fromx fromy toy) image = verticalLine fromx fromy toy image

runLineQueries :: Vector LineQuery -> Image Pixel8 -> Vector (Vector Pixel8)
runLineQueries linequeries image = Vector.map (flip runLineQuery image) linequeries

averageImageFold :: Bool -> Fold (Image Pixel8) (Maybe (Image PixelF))
averageImageFold False = Fold const () (const Nothing)
averageImageFold True = Fold addImage Nothing id

addImage :: Maybe (Image PixelF) -> Image Pixel8 -> Maybe (Image PixelF)
addImage = undefined
