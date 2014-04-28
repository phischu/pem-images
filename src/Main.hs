module Main where

import ImageLoading (imageSeries)
import ImageQuery (
    ImageQueryStatement(SetImageQueryParameter,GetImageQueryResult),
    ImageQuery(TableQuery,IslandImage),
    ImageQueryParameter(Threshold),
    TableQuery(..),
    IslandQuery(..),
    Polarity(Dark,Bright),
    runImageQueries,ImageQueryResult(..))
import ImageProcessing (imageToJuicy,identityStencil)

import Codec.Picture (writeBitmap)

import Graphics.UI.WX (
    start,frameLoadRes,Prop((:=)),sz,clientSize)
import Graphics.UI.WXCore (
    windowShow)

import Pipes (runEffect,(>->),for)
import Control.Foldl (purely)
import Pipes.Prelude (fold)
import qualified Pipes.Prelude as Pipes

import Data.Traversable (forM)
import Control.Monad (forever,(>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State (evalStateT,get,put)

import Control.Error (EitherT,runEitherT)
import Data.Vector (Vector)
import Data.Vector as V (fromList,indexed)
import qualified Data.ByteString.Lazy as ByteString (writeFile)

import qualified Data.Csv as Csv (encode)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Codec.Picture (writeBitmap)
import Data.IORef (IORef,newIORef,readIORef,writeIORef)

testdirectory :: FilePath
testdirectory = "data/small_dark_islands/"

testqueries :: [ImageQueryStatement]
testqueries = [
    SetImageQueryParameter (Threshold 14),
    GetImageQueryResult (TableQuery (IslandQuery Bright NumberOfIslands)),
    GetImageQueryResult (TableQuery (IslandQuery Bright  AverageOutlineOfIslands)),
    GetImageQueryResult (IslandImage Dark),
    SetImageQueryParameter (Threshold 251),
    GetImageQueryResult (IslandImage Bright),
    GetImageQueryResult (TableQuery (IslandQuery Dark NumberOfIslands)),
    GetImageQueryResult (TableQuery (AverageAroundPoint 2 126 12))]

differentThresholds :: [ImageQueryStatement]
differentThresholds = do
    threshold <- [0,8..255]
    [SetImageQueryParameter (Threshold threshold),GetImageQueryResult (IslandImage Dark)]

saveResult :: (MonadIO m) => IORef Int -> ImageQueryResult -> m ()
saveResult countref imagequeryresult = liftIO (do
    c <- readIORef countref
    writeIORef countref (c+1)
    forM (zip [0..] (_outputImages imagequeryresult)) (\(i,image) -> do
        writeBitmap ("result" </> "intermediateimages" </> "thresholded-" ++ show i ++ "-" ++ show c ++ ".bmp") (imageToJuicy image))
    print (_tableRow imagequeryresult))

main :: IO ()
main = runBatch

gui :: IO ()
gui = start (do
    f <- frameLoadRes "GUI.xrc" "MainFrame" []
    windowShow f
    return ())

runBatch :: IO ()
runBatch = do
    createDirectoryIfMissing True "result"
    createDirectoryIfMissing True ("result" </> "intermediateimages")
    countref <- newIORef 0
    result <- runEitherT (runEffect (for (imageSeries testdirectory) (runImageQueries testqueries >=> saveResult countref)))
    case result of
        Left err -> print err
        Right () -> print "alright"