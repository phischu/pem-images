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
import ImageProcessing (imageToJuicy)
import ImageQuery.Parser (imageQueriesParser)

import Codec.Picture (writeBitmap)

import Graphics.UI.WX (
    start,frameLoadRes)
import Graphics.UI.WXCore (
    windowShow)

import Pipes (runEffect,for)

import Data.Traversable (forM)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO,liftIO)

import Control.Error (EitherT,runEitherT,scriptIO,hoistEither,fmapLT)

import Text.Parsec.String (parseFromFile)

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
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
        writeBitmap (intermediateImagePath i c) (imageToJuicy image))
    print (_tableRow imagequeryresult))

intermediateImagePath :: Int -> Int -> FilePath
intermediateImagePath i c = "result" </> "intermediateimages" </> "thresholded-" ++ show i ++ "-" ++ show c ++ ".bmp"

main :: IO ()
main = runBatch "test.imagequery" >>= either putStrLn (const (putStrLn "success"))

gui :: IO ()
gui = start (do
    f <- frameLoadRes "GUI.xrc" "MainFrame" []
    windowShow f
    return ())

runBatch :: FilePath -> IO (Either String ())
runBatch queryfilename = runEitherT (do
    parseresult <- scriptIO (parseFromFile imageQueriesParser queryfilename)
    imagequerystatements <- hoistEither parseresult `onFailure` show
    scriptIO (createDirectoryIfMissing True "result")
    scriptIO (createDirectoryIfMissing True ("result" </> "intermediateimages"))
    countref <- scriptIO (newIORef 0)
    runEffect (for (imageSeries testdirectory) (
        runImageQueries imagequerystatements >=> saveResult countref)) `onFailure` show)

onFailure :: (Monad m) => EitherT a m b -> (a -> c) -> EitherT c m b
onFailure = flip fmapLT
