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
import ImageProcessing (Image,imageToJuicy)
import ImageQuery.Parser (imageQueriesParser)

import Codec.Picture (Pixel8,writeBitmap)

import Graphics.UI.WX (
    start,frameLoadRes)
import Graphics.UI.WXCore (
    windowShow)

import Pipes (Consumer,runEffect,(>->),await)
import qualified Pipes.Prelude as Pipes (mapM)

import Data.Traversable (forM)
import Control.Monad.IO.Class (MonadIO,liftIO)

import Control.Error (EitherT,runEitherT,scriptIO,hoistEither,fmapLT)

import Text.Parsec.String (parseFromFile)

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

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

consumeResults :: (MonadIO m) => Consumer ImageQueryResult m r
consumeResults = go 1 where
    go n = do
        imagequeryresult <- await
        liftIO (do
            saveIntermediateImages n (_outputImages imagequeryresult)
            print (_tableRow imagequeryresult))
        go (n+1)

saveIntermediateImages :: Int -> [Image Pixel8] -> IO [()]
saveIntermediateImages n outputimages = liftIO (forM (zip [0..] outputimages) (\(i,image) -> do
    writeBitmap (intermediateImagePath i n) (imageToJuicy image)))

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
    runEffect (
        imageSeries testdirectory >->
        Pipes.mapM (runImageQueries imagequerystatements) >->
        consumeResults) `onFailure` show)

onFailure :: (Monad m) => EitherT a m b -> (a -> c) -> EitherT c m b
onFailure = flip fmapLT
