module ImageQuery.Parser where

import ImageQuery (
    ImageQueryStatement(SetImageQueryParameter,GetImageQueryResult),
    ImageQueryParameter(Threshold),
    ImageQuery(TableQuery,IslandImage,ImageOfAverage,LineImage),
    TableQuery(ValueInPoint,AverageAroundPoint,AverageOfImage,IslandQuery),
    IslandQuery(NumberOfIslands,AverageAreaOfIslands,AverageOutlineOfIslands),
    Polarity(Bright,Dark),
    Orientation(Horizontal,Vertical))
import Text.Parsec (
    sepEndBy,newline,choice,try,string,spaces,digit,many1,(<|>))
import Text.Parsec.String (Parser)

imageQueriesParser :: Parser [ImageQueryStatement]
imageQueriesParser = imageQueryParser `sepEndBy` newline

imageQueryParser :: Parser ImageQueryStatement
imageQueryParser = choice [setPropertyParser,tableQueryParser,outputParser]

setPropertyParser :: Parser ImageQueryStatement
setPropertyParser = choice (map try (map (fmap SetImageQueryParameter) [
    thresholdParser]))

thresholdParser :: Parser ImageQueryParameter
thresholdParser = do
    string "set_threshold"
    spaces
    thresholdstring <- digits
    return (Threshold (read thresholdstring))

tableQueryParser :: Parser ImageQueryStatement
tableQueryParser = choice (map try (map (fmap (GetImageQueryResult . TableQuery)) [
    valueInPointParser,
    averageAroundPointParser,
    averageOfImageParser,
    numberOfIslandsParser,
    averageAreaOfIslandsParser,
    averageOutlineOfIslandsParser]))

valueInPointParser :: Parser TableQuery
valueInPointParser = do
    string "table_value_in_point"
    spaces
    xstring <- digits
    spaces
    ystring <- digits
    return (ValueInPoint (read xstring) (read ystring))

averageAroundPointParser :: Parser TableQuery
averageAroundPointParser = do
    string "table_average_around_point"
    spaces
    xstring <- digits
    spaces
    ystring <- digits
    spaces
    rstring <- digits
    return (AverageAroundPoint (read xstring) (read ystring) (read rstring))

averageOfImageParser :: Parser TableQuery
averageOfImageParser = do
    string "table_average_of_image"
    return AverageOfImage

numberOfIslandsParser :: Parser TableQuery
numberOfIslandsParser = do
    string "table_number_of_islands"
    spaces
    polarity <- polarityParser
    return (IslandQuery polarity NumberOfIslands)

averageAreaOfIslandsParser :: Parser TableQuery
averageAreaOfIslandsParser = do
    string "table_average_area_of_islands"
    spaces
    polarity <- polarityParser
    return (IslandQuery polarity AverageAreaOfIslands)

averageOutlineOfIslandsParser :: Parser TableQuery
averageOutlineOfIslandsParser = do
    string "table_average_outline_of_islands"
    spaces
    polarity <- polarityParser
    return (IslandQuery polarity AverageOutlineOfIslands)

outputParser :: Parser ImageQueryStatement
outputParser = choice (map try (map (fmap GetImageQueryResult) [
    islandImagesParser,
    averageImageParser,
    lineImageParser]))

islandImagesParser :: Parser ImageQuery
islandImagesParser = do
    string "output_island_images"
    spaces
    polarity <- polarityParser
    return (IslandImage polarity)

averageImageParser :: Parser ImageQuery
averageImageParser = do
    string "output_average_image"
    return ImageOfAverage

lineImageParser :: Parser ImageQuery
lineImageParser = do
    string "output_line_image"
    spaces
    orientation <- orientationParser
    spaces
    xstring <- digits
    spaces
    ystring <- digits
    spaces
    lstring <- digits
    return (LineImage orientation (read xstring) (read ystring) (read lstring))

orientationParser :: Parser Orientation
orientationParser = (string "horizontal" >> return Horizontal) <|> (string "vertical" >> return Vertical)

polarityParser :: Parser Polarity
polarityParser = (string "bright" >> return Bright) <|> (string "dark" >> return Dark)

printImageQueries :: [ImageQueryStatement] -> String
printImageQueries = undefined

digits :: Parser String
digits = many1 digit
