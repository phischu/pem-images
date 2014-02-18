module Main where

import ImageLoading (imageSeries)
import ImageQuery (ImageQuery(ValueInPoint),runImageQueries)

import Pipes (runEffect,(>->))
import qualified Pipes.Prelude as Pipes
import Data.Vector (Vector)
import Data.Vector as V (fromList)
import Control.Error (runEitherT)

testdirectory :: FilePath
testdirectory = "data/2008-05/PEEM08050800/"

testqueries :: Vector ImageQuery
testqueries = V.fromList [ValueInPoint 20 20]

main :: IO ()
main = runEitherT (runEffect (
    imageSeries testdirectory >->
    Pipes.map (runImageQueries testqueries) >->
    Pipes.print)) >>= print
