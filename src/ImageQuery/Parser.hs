module ImageQuery.Parser where

import ImageQuery (
    ImageQueryStatement(SetImageQueryParameter,GetImageQueryResult),
    ImageQueryParameter(Threshold),
    ImageQuery(TableQuery,IslandImage),
    TableQuery(ValueInPoint),
    Polarity(Bright,Dark))
import Text.Parsec (
    sepBy,newline,choice,try,string,spaces,digit,many1,(<|>))
import Text.Parsec.String (Parser)

imageQueriesParser :: Parser [ImageQueryStatement]
imageQueriesParser = imageQueryParser `sepBy` newline

imageQueryParser :: Parser ImageQueryStatement
imageQueryParser = choice [setPropertyParser,tableQuueryParser,outputParser]

setPropertyParser :: Parser ImageQueryStatement
setPropertyParser = choice (map try (map (fmap SetImageQueryParameter) [
    thresholdParser]))

thresholdParser :: Parser ImageQueryParameter
thresholdParser = do
    string "set_threshold"
    spaces
    thresholdstring <- digits
    return (Threshold (read thresholdstring))

tableQuueryParser :: Parser ImageQueryStatement
tableQuueryParser = choice (map try (map (fmap (GetImageQueryResult . TableQuery)) [
    valueInPointParser]))

valueInPointParser :: Parser TableQuery
valueInPointParser = do
    string "table_value_in_point"
    spaces
    xstring <- digits
    spaces
    ystring <- digits
    return (ValueInPoint (read xstring) (read ystring))

outputParser :: Parser ImageQueryStatement
outputParser = choice (map try (map (fmap GetImageQueryResult) [
    islandImagesParser]))

islandImagesParser :: Parser ImageQuery
islandImagesParser = do
    string "output_island_images"
    spaces
    polarity <- brightOrDarkParser
    return (IslandImage polarity)

brightOrDarkParser :: Parser Polarity
brightOrDarkParser = (string "bright" >> return Bright) <|> (string "dark" >> return Dark)

printImageQueries :: [ImageQueryStatement] -> String
printImageQueries = undefined

digits :: Parser String
digits = many1 digit
