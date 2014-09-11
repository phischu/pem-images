module Main where

import GUI (gui)
import ImageQuery (
    ImageQueryStatement(SetImageQueryParameter,GetImageQueryResult),
    ImageQuery(TableQuery,IslandImage,LineImage,ImageOfAverage),
    ImageQueryParameter(Threshold),
    Orientation(Horizontal),
    TableQuery(..),
    IslandQuery(..))

testdirectory :: FilePath
testdirectory = "data/small_dark_islands/"

testqueries :: [ImageQueryStatement]
testqueries = [
    SetImageQueryParameter (Threshold 14),
    GetImageQueryResult (TableQuery (IslandQuery NumberOfIslands)),
    GetImageQueryResult (TableQuery (IslandQuery  AverageOutlineOfIslands)),
    GetImageQueryResult IslandImage,
    SetImageQueryParameter (Threshold 251),
    GetImageQueryResult IslandImage,
    GetImageQueryResult (TableQuery (IslandQuery NumberOfIslands)),
    GetImageQueryResult (TableQuery (AverageAroundPoint 2 126 12)),
    GetImageQueryResult ImageOfAverage,
    GetImageQueryResult (LineImage Horizontal 5 6 300)]

differentThresholds :: [ImageQueryStatement]
differentThresholds = do
    threshold <- [0,8..255]
    [SetImageQueryParameter (Threshold threshold),GetImageQueryResult IslandImage]

main :: IO ()
main = gui
