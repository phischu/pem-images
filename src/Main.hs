module Main where

import ImageLoading (imageSeries)
import ImageQuery (
    ImageQuery(ImageQuery),
    TableQuery(
        NumberOfIslands,ValueInPoint,AverageAroundPoint,
        AverageOfImage,AverageAreaOfIslands,AverageOutlineOfIslands),
    LineQuery(HorizontalLine),
    runImageQuery,ImageQueryResult(tableRows,lineImages,averageImage))
import ImageProcessing (imageToJuicy,identityStencil)

import Codec.Picture (writeBitmap)

import Pipes ((>->))
import Control.Foldl (purely)
import Pipes.Prelude (fold)
import qualified Pipes.Prelude as Pipes

import Data.Traversable (forM)

import Control.Error (EitherT,runEitherT)
import Data.Vector (Vector)
import Data.Vector as V (fromList,indexed)
import qualified Data.ByteString.Lazy as ByteString (writeFile)

import qualified Data.Csv as Csv (encode)

testdirectory :: FilePath
testdirectory = "data/2008-05/PEEM08050800/"

testtablequeries :: Vector TableQuery
testtablequeries = V.fromList [
    NumberOfIslands,
    AverageAroundPoint 2 126 12,
    ValueInPoint 2 126,
    AverageOfImage,
    AverageAreaOfIslands,
    AverageOutlineOfIslands]

testquery :: ImageQuery
testquery = ImageQuery (identityStencil 1080 1032) 20 testtablequeries (V.fromList [HorizontalLine (-3) 14 12]) True

main :: IO ()
main = do
    result <- runEitherT (purely fold (runImageQuery testquery)
        (imageSeries testdirectory >-> Pipes.take 2))
    case result of
        Left err -> print err
        Right imagequeryresult -> do
            ByteString.writeFile "result.txt" (Csv.encode (tableRows imagequeryresult))
            forM (V.indexed (lineImages imagequeryresult)) (\(i,image) -> do
                writeBitmap ("lineimage" ++ show i ++ ".bmp") (imageToJuicy image))
            maybe (putStrLn "no average image") (writeBitmap "average_image.bmp" . imageToJuicy) (averageImage imagequeryresult)
