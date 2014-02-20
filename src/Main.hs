module Main where

import ImageLoading (imageSeries)
import ImageQuery (
    ImageQuery(ImageQuery),TableQuery(ValueInPoint),
    runImageQuery,ImageQueryResult(tableRows))

import Pipes ((>->))
import Control.Foldl (purely)
import Pipes.Prelude (fold)
import qualified Pipes.Prelude as Pipes
import Data.Vector as V (fromList)
import Control.Error (EitherT,runEitherT)

testdirectory :: FilePath
testdirectory = "data/2008-05/PEEM08050800/"

testquery :: ImageQuery
testquery = ImageQuery (V.fromList [ValueInPoint 20 20]) (V.fromList []) False

main :: IO ()
main = runEitherT (purely fold (runImageQuery testquery) (
    imageSeries testdirectory >-> Pipes.take 20)) >>= either print (print . tableRows)
