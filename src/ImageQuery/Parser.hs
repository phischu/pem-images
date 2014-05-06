module ImageQuery.Parser where

import ImageQuery (ImageQueryStatement)

parseImageQueries :: String -> Either String [ImageQueryStatement]
parseImageQueries imagequeriesstring = mapM parseImageQuery (lines imagequeriesstring)

parseImageQuery :: String -> Either String ImageQueryStatement
parseImageQuery = undefined

printImageQueries :: [ImageQueryStatement] -> String
printImageQueries = undefined
