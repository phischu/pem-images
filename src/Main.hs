module Main where

import ImageLoading (imageSeries)
import ImageQuery (
    ImageQueryStatement(SetImageQueryParameter,GetImageQueryResult),
    ImageQuery(TableQuery),
    ImageQueryParameter(Threshold),
    TableQuery(..),
    Polarity(Dark,Bright),
    runImageQueries)
import ImageProcessing (imageToJuicy,identityStencil)

import Codec.Picture (writeBitmap)

import Pipes ((>->),Pipe,await,yield)
import Control.Foldl (purely)
import Pipes.Prelude (fold)
import qualified Pipes.Prelude as Pipes

import Data.Traversable (forM)
import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State (evalStateT,get,put)

import Control.Error (EitherT,runEitherT)
import Data.Vector (Vector)
import Data.Vector as V (fromList,indexed)
import qualified Data.ByteString.Lazy as ByteString (writeFile)

import qualified Data.Csv as Csv (encode)

testdirectory :: FilePath
testdirectory = "data/2008-05/testimages/"

testtablequeries :: [ImageQueryStatement]
testtablequeries = [
    SetImageQueryParameter (Threshold 14),
    GetImageQueryResult (NumberOfIslands Bright),
    GetImageQueryResult (AverageOutlineOfIslands Bright),
    GetImageQueryResult (NumberOfIslands Dark),
    GetImageQueryResult (AverageAroundPoint 2 126 12)]

imageQueryResultConsumer :: Consumer ImageQueryResult m ()
imageQueryResultConsumer = undefined

main :: IO ()
main = do
    result <- runEitherT (purely fold (runImageQuery testquery)
        (imageSeries testdirectory >-> progress))
    case result of
        Left err -> print err
        Right imagequeryresult -> do
            ByteString.writeFile "result.txt" (Csv.encode (tableRows imagequeryresult))
            forM (V.indexed (lineImages imagequeryresult)) (\(i,image) -> do
                writeBitmap ("lineimage" ++ show i ++ ".bmp") (imageToJuicy image))
            maybe (putStrLn "no average image") (writeBitmap "average_image.bmp" . imageToJuicy) (averageImage imagequeryresult)
