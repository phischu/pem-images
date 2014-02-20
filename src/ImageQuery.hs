module ImageQuery where

import ImageProcessing (valueInPoint)

import Codec.Picture (Image,Pixel8,PixelF)

import Control.Foldl (Fold(Fold),list,premap)
import Control.Applicative ((<$>),(<*>))

import Data.Vector (Vector)
import Data.Vector as V (map)

data ImageQuery = ImageQuery {
    tableQueries :: Vector TableQuery,
    lineQuery :: Vector LineQuery,
    averageImageQuery :: Bool} deriving Show
    
data TableQuery =
    ValueInPoint Int Int deriving Show

data LineQuery =
    HorizontalLine {
        start :: Int,
        end :: Int,
        width:: Int} |
    VerticalLine {
        start :: Int,
        end :: Int,
        height :: Int} deriving Show

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
runTableQueries tablequeries image = V.map (flip runTableQuery image) tablequeries

lineFold :: Vector LineQuery -> Fold (Image Pixel8) [Vector (Vector Pixel8)]
lineFold linequeries = premap (runLineQueries linequeries) list

runLineQuery :: LineQuery -> Image Pixel8 -> Vector Pixel8
runLineQuery = undefined

runLineQueries :: Vector LineQuery -> Image Pixel8 -> Vector (Vector Pixel8)
runLineQueries = undefined

averageImageFold :: Bool -> Fold (Image Pixel8) (Maybe (Image PixelF))
averageImageFold False = Fold const () (const Nothing)
averageImageFold True = Fold addImage Nothing id

addImage :: Maybe (Image PixelF) -> Image Pixel8 -> Maybe (Image PixelF)
addImage = undefined
