module Main where

import ImageLoading (imageSeries)
import ImageQuery (
    ImageQuery(ImageQuery),
    TableQuery(
        ValueInPoint,AverageAroundPoint,AverageOfImage,
        IslandQuery),
    Polarity(
        Dark, Bright),
    IslandQuery(
        NumberOfIslands,AverageAreaOfIslands,AverageOutlineOfIslands),
    LineQuery(HorizontalLine),
    runImageQuery,ImageQueryResult(tableRows,lineImages,averageImage))
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

testtablequeries :: Vector TableQuery
testtablequeries = V.fromList [
    IslandQuery Bright NumberOfIslands,
    AverageAroundPoint 2 126 12,
    ValueInPoint 2 126,
    AverageOfImage,
    IslandQuery Bright AverageAreaOfIslands,
    IslandQuery Bright AverageOutlineOfIslands,
    IslandQuery Dark NumberOfIslands]

testquery :: ImageQuery
testquery = ImageQuery (0,0,1080,1032) (identityStencil 1080 1032) 20 testtablequeries (V.fromList [HorizontalLine (-3) 14 12]) True

progress :: (MonadIO m) => Pipe a a m r
progress = flip evalStateT 1 (forever(do
    n <- get
    liftIO (putStrLn ("Image: " ++ show n))
    put (n+1)
    lift (await >>= yield)))

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
