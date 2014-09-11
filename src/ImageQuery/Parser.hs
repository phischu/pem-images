module ImageQuery.Parser where

import ImageQuery (
    ImageQueryStatement(SetImageQueryParameter,GetImageQueryResult),
    ImageQueryParameter(Threshold,Channel,Smoothing,SubRect,StencilImage,Polarity),
    ImageQuery(TableQuery,IslandImage,ImageOfAverage,LineImage,AreaHistogram),
    TableQuery(ValueInPoint,AverageAroundPoint,AverageOfImage,IslandQuery),
    IslandQuery(NumberOfIslands,AverageAreaOfIslands,AverageOutlineOfIslands),
    Polarity(Bright,Dark),
    Orientation(Horizontal,Vertical),
    Channel(Red,Green,Blue),
    Power(One,OneOverTwo,ThreeOverTwo))
import Text.Parsec (
    sepEndBy,newline,choice,try,string,spaces,digit,many1,(<|>),manyTill,anyChar)
import Text.Parsec.String (Parser)
import Control.Monad (replicateM)

imageQueriesParser :: Parser [ImageQueryStatement]
imageQueriesParser = imageQueryParser `sepEndBy` newline

imageQueryParser :: Parser ImageQueryStatement
imageQueryParser = choice [setPropertyParser,tableQueryParser,outputParser]

setPropertyParser :: Parser ImageQueryStatement
setPropertyParser = choice (map try (map (fmap SetImageQueryParameter) [
    thresholdParser,
    channelParser,
    smoothingParser,
    subrectParser,
    stencilParser,
    polarityParser]))

thresholdParser :: Parser ImageQueryParameter
thresholdParser = do
    string "set_threshold"
    spaces
    thresholdstring <- digits
    return (Threshold (read thresholdstring))

channelParser :: Parser ImageQueryParameter
channelParser = do
    string "set_channel"
    spaces
    channeltype <- channelTypeParser
    return (Channel channeltype)

smoothingParser :: Parser ImageQueryParameter
smoothingParser = do
    string "set_smoothing"
    spaces
    smoothingstring <- digits
    return (Smoothing (read smoothingstring))

subrectParser :: Parser ImageQueryParameter
subrectParser = do
    string "set_subrect"
    [x,y,w,h] <- replicateM 4 (do
        spaces
        numberstring <- digits
        return (read numberstring))
    return (SubRect (x,y,w,h))

stencilParser :: Parser ImageQueryParameter
stencilParser = do
    string "set_stencil"
    spaces
    stencilFilePath <- manyTill anyChar newline
    return (StencilImage stencilFilePath Nothing)

polarityParser :: Parser ImageQueryParameter
polarityParser = do
    string "set_polarity"
    spaces
    polarity <- polarityTypeParser
    return (Polarity polarity)

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
    return (IslandQuery NumberOfIslands)

averageAreaOfIslandsParser :: Parser TableQuery
averageAreaOfIslandsParser = do
    string "table_average_area_of_islands"
    return (IslandQuery AverageAreaOfIslands)

averageOutlineOfIslandsParser :: Parser TableQuery
averageOutlineOfIslandsParser = do
    string "table_average_outline_of_islands"
    return (IslandQuery AverageOutlineOfIslands)

outputParser :: Parser ImageQueryStatement
outputParser = choice (map try (map (fmap GetImageQueryResult) [
    islandImagesParser,
    averageImageParser,
    lineImageParser,
    areaHistogramParser]))

islandImagesParser :: Parser ImageQuery
islandImagesParser = do
    string "output_island_images"
    return IslandImage

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

areaHistogramParser :: Parser ImageQuery
areaHistogramParser = do
    string "output_area_histogram"
    spaces
    binsize <- digits
    spaces
    power <- powerParser
    return (AreaHistogram (read binsize) power)

orientationParser :: Parser Orientation
orientationParser = (string "horizontal" >> return Horizontal) <|> (string "vertical" >> return Vertical)

polarityTypeParser :: Parser Polarity
polarityTypeParser = (string "bright" >> return Bright) <|> (string "dark" >> return Dark)

channelTypeParser :: Parser Channel
channelTypeParser =
    (string "red" >> return Red) <|>
    (string "green" >> return Green) <|>
    (string "blue" >> return Blue)

powerParser :: Parser Power
powerParser =
    (string "one" >> return One) <|>
    (string "one_over_two" >> return OneOverTwo) <|>
    (string "three_over_two" >> return ThreeOverTwo)

digits :: Parser String
digits = many1 digit
