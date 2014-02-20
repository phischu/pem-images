module ImageQuery where

import ImageProcessing (valueInPoint)

import Codec.Picture (Image,Pixel8,PixelF)

import Control.Foldl (Fold)

import Data.Vector (Vector)
import Data.Vector as V (map)

data ImageQuery = ImageQuery {
    tableQueries :: Vector TableQuery,
    lineQuery :: LineQuery,
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
    lineValues :: [Vector Pixel8],
    averageImage :: (Maybe (Image PixelF))}

runImageQuery :: ImageQuery -> Fold (Image Pixel8) ImageQueryResult
runImageQuery = undefined

runTableQuery :: TableQuery -> Image Pixel8 -> Double
runTableQuery (ValueInPoint x y) image = valueInPoint x y image

runTableQueries :: Vector TableQuery -> Image Pixel8 -> Vector Double
runTableQueries tablequeries image = V.map (flip runTableQuery image) tablequeries

runLineQuery = undefined

addImage = undefined
