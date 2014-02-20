module ImageQuery where

import ImageProcessing (valueInPoint)

import Codec.Picture (Image,Pixel8,PixelF)

import Pipes (Consumer)
import Control.Monad.Trans.State (StateT)

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

data ImageQueryAccumulator = ImageQueryAccumulator {
    tableRows :: [Vector Double],
    lineValues :: [Vector Pixel8],
    sumImage :: Image PixelF,
    numberOfImages :: Integer}

data ImageQueryResult = ImageQueryResult [Vector Double] [Vector Pixel8] (Maybe (Image PixelF))

runImageQuery :: ImageQuery -> Consumer (Image Pixel8) (StateT ImageQueryAccumulator m) ()
runImageQuery = undefined

imageQueryStep :: ImageQuery -> ImageQueryAccumulator -> Image Pixel8 -> ImageQueryAccumulator
imageQueryStep imagequery accumulator image = ImageQueryAccumulator
    (runTableQueries (tableQueries imagequery) image : tableRows accumulator)
    (runLineQuery (lineQuery imagequery) image : lineValues accumulator)
    (addImage (sumImage accumulator) image)
    (numberOfImages accumulator + 1)

finalizeImageQuery :: ImageQueryAccumulator -> ImageQueryResult
finalizeImageQuery = undefined

runTableQuery :: TableQuery -> Image Pixel8 -> Double
runTableQuery (ValueInPoint x y) image = valueInPoint x y image

runTableQueries :: Vector TableQuery -> Image Pixel8 -> Vector Double
runTableQueries tablequeries image = V.map (flip runTableQuery image) tablequeries

runLineQuery = undefined

addImage = undefined
