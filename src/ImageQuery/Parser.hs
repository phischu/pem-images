module ImageQuery.Parser where

import ImageQuery (ImageQueryStatement)
import Text.Parsec.String (Parser)

imagequeriesparser :: Parser [ImageQueryStatement]
imagequeriesparser = undefined

parseImageQuery :: String -> Either String ImageQueryStatement
parseImageQuery = undefined

printImageQueries :: [ImageQueryStatement] -> String
printImageQueries = undefined
