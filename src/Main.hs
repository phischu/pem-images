module Main where

import ImageLoading (imageSeries)
import ImageQuery (
    ImageQueryStatement(SetImageQueryParameter,GetImageQueryResult),
    ImageQuery(TableQuery,IslandImage,ImageOfAverage),
    ImageQueryParameter(Threshold),
    TableQuery(..),
    IslandQuery(..),
    Polarity(Dark,Bright),
    runImageQueries,ImageQueryResult(..))
import ImageProcessing (
    Image,imageToJuicy,
    singleAverageImage,addImage,finalizeAverageImage,
    singleLineImage,appendLine)
import ImageQuery.Parser (imageQueriesParser)

import Codec.Picture (Pixel8,writeBitmap)

import Graphics.UI.WX (
    start,frameLoadRes)
import Graphics.UI.WXCore (
    windowShow)

import Pipes (Consumer,runEffect,(>->),await)
import qualified Pipes.Prelude as Pipes (mapM)

import Control.Monad (forever,forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State (StateT,evalStateT,get,put)

import Control.Error (EitherT,runEitherT,scriptIO,hoistEither,fmapLT)

import Text.Parsec.String (parseFromFile)

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (Handle,hPutStrLn,openFile,hClose,IOMode(WriteMode))

import Data.Maybe (listToMaybe)

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
    GetImageQueryResult (TableQuery (AverageAroundPoint 2 126 12)),
    GetImageQueryResult ImageOfAverage]

differentThresholds :: [ImageQueryStatement]
differentThresholds = do
    threshold <- [0,8..255]
    [SetImageQueryParameter (Threshold threshold),GetImageQueryResult (IslandImage Dark)]

consumeResults :: (MonadIO m) => Handle -> Consumer ImageQueryResult m r
consumeResults tablehandle = flip evalStateT (0,Nothing,Nothing) (forever (do

    imagequeryresult <- lift await
    (n,maybeaverageimage,maybelineimages) <- get

    let maybeaverageimage' = do
            image <- listToMaybe (_averageImages imagequeryresult)
            case maybeaverageimage of
                Nothing -> Just (singleAverageImage image)
                Just averageimage -> Just (addImage averageimage image)
        n' = n+1
        imagelines = _imageLines imagequeryresult
        maybelineimages' = case maybelineimages of
            Nothing -> Just (map singleLineImage imagelines)
            Just lineimages -> Just (zipWith appendLine lineimages imagelines)

    put (n',maybeaverageimage',maybelineimages')

    liftIO (do
        saveIntermediateImages n (_outputImages imagequeryresult)
        saveTableRow tablehandle (_tableRow imagequeryresult)
        saveAverageImage (finalizeAverageImage maybeaverageimage' n')
        maybe (return ()) saveLineImages maybelineimages')))

saveIntermediateImages :: Int -> [Image Pixel8] -> IO ()
saveIntermediateImages n outputimages = forM_ (zip [0..] outputimages) (\(i,image) -> do
    writeBitmap (intermediateImagePath i n) (imageToJuicy image))

intermediateImagePath :: Int -> Int -> FilePath
intermediateImagePath i c = "result" </> "intermediateimages" </> "thresholded-" ++ show i ++ "-" ++ show c ++ ".bmp"

saveTableRow :: Handle -> [Double] -> IO ()
saveTableRow tablehandle tablerow = hPutStrLn tablehandle (csvRow tablerow)

csvRow :: [Double] -> String
csvRow = unwords . map show

csvResultPath :: FilePath
csvResultPath = "result" </> "table.csv"

saveAverageImage :: Maybe (Image Pixel8) -> IO ()
saveAverageImage = maybe (return ()) (writeBitmap averageImagePath . imageToJuicy)

averageImagePath :: FilePath
averageImagePath = "result" </> "averageimage.bmp"

saveLineImages :: [Image Pixel8] -> IO ()
saveLineImages lineimages = forM_ (zip [0..] lineimages) (\(i,lineimage) -> do
    writeBitmap (lineImagePath i) (imageToJuicy lineimage))

lineImagePath :: Int -> FilePath
lineImagePath i = "result" </> "lineimage-" ++ show i ++ ".bmp"

main :: IO ()
main = runEitherT (runBatch testqueries) >>= either putStrLn (const (putStrLn "success"))

gui :: IO ()
gui = start (do
    f <- frameLoadRes "GUI.xrc" "MainFrame" []
    windowShow f
    return ())

run :: FilePath -> EitherT String IO ()
run queryfilename = do
    parseresult <- scriptIO (parseFromFile imageQueriesParser queryfilename)
    imagequerystatements <- hoistEither parseresult `onFailure` show
    runBatch imagequerystatements

runBatch :: [ImageQueryStatement] -> EitherT String IO ()
runBatch imagequerystatements = do
    scriptIO (createDirectoryIfMissing True "result")
    scriptIO (createDirectoryIfMissing True ("result" </> "intermediateimages"))
    tablehandle <- scriptIO (openFile csvResultPath WriteMode)
    runEffect (
        imageSeries testdirectory >->
        Pipes.mapM (runImageQueries imagequerystatements) >->
        consumeResults tablehandle) `onFailure` show
    scriptIO (hClose tablehandle)

onFailure :: (Monad m) => EitherT a m b -> (a -> c) -> EitherT c m b
onFailure = flip fmapLT
