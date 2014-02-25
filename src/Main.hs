module Main where

import ImageLoading (imageSeries)
import ImageQuery (
    ImageQuery(ImageQuery),TableQuery(ValueInPoint),LineQuery(HorizontalLine),
    runImageQuery,ImageQueryResult(tableRows,lineValues,averageImage))

import Codec.Picture (writeBitmap)

import Pipes ((>->))
import Control.Foldl (purely)
import Pipes.Prelude (fold)
import qualified Pipes.Prelude as Pipes
import Data.Vector as V (fromList)
import Control.Error (EitherT,runEitherT)

testdirectory :: FilePath
testdirectory = "data/2008-05/PEEM08050800/"

testquery :: ImageQuery
testquery = ImageQuery (V.fromList [ValueInPoint 20 20]) (V.fromList [HorizontalLine (-3) 14 12]) True

main :: IO ()
main = do
    result <- runEitherT (purely fold (runImageQuery testquery)
        (imageSeries testdirectory >-> Pipes.take 20))
    case result of
        Left err -> print err
        Right imagequeryresult -> do
            print (lineValues imagequeryresult)
            print (tableRows imagequeryresult)
            maybe (putStrLn "no average image") (writeBitmap "average_image.bmp") (averageImage imagequeryresult)
