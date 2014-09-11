module Run where

import ImageLoading (
    imageSeries)
import ImageQuery (
    ImageQueryStatement,runImageQueries,
    ImageQueryParameters(_channel,_smoothing,_threshold,_polarity),
    ImageQueryResult(_averageImages,_imageLines,_outputImages,_tableRow,_histograms),
    Power)
import ImageProcessing (
    Image,imageToJuicy,
    singleAverageImage,addImage,finalizeAverageImage,
    singleLineImage,appendLine)
import ImageQuery.Printer (channelPrinter,polarityPrinter)

import Codec.Picture (Pixel8,writeBitmap)

import Pipes (Consumer,runEffect,(>->),await)
import qualified Pipes.Prelude as Pipes (mapM)

import Control.Monad (forever)
import Data.Foldable (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State (evalStateT,get,put)

import Control.Error (EitherT,runEitherT,scriptIO,fmapLT)

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>),takeBaseName)
import System.IO (Handle,hPutStrLn,openFile,hClose,IOMode(WriteMode))

import Data.Maybe (listToMaybe)
import Data.List (intercalate)

run :: FilePath -> [ImageQueryStatement] -> IO (Either String ())
run inputdirectory imagequerystatements = runEitherT (do
    let inputbasename = takeBaseName inputdirectory
        resultsPath = "results" </> inputbasename
    scriptIO (createDirectoryIfMissing True (resultsPath </> islandImagesPath))
    scriptIO (createDirectoryIfMissing True (resultsPath </> histogramsPath))
    tablehandle <- scriptIO (openFile (resultsPath </> tablePath) WriteMode)
    runEffect (
        imageSeries inputdirectory >->
        Pipes.mapM (runImageQueries imagequerystatements) >->
        consumeResults resultsPath tablehandle) `onFailure` show
    scriptIO (hClose tablehandle))

islandImagesPath :: FilePath
islandImagesPath = "islandimages"

islandImagePath :: Int -> FilePath
islandImagePath i = "islandimage-" ++ show i ++ ".bmp"

histogramsPath :: FilePath
histogramsPath = "histograms"

histogramPath :: Int -> FilePath
histogramPath i = "histogram-" ++ show i ++ ".csv"

averageImagePath :: FilePath
averageImagePath = "averageimage.bmp"

lineImagePath :: Int -> FilePath
lineImagePath i = "lineimage-" ++ show i ++ ".bmp"

tablePath :: FilePath
tablePath = "table.csv"

parametersPath :: ImageQueryParameters -> FilePath
parametersPath imagequeryparameters = intercalate "-" [
    channelPrinter (_channel imagequeryparameters),
    show (_smoothing imagequeryparameters),
    show (_threshold imagequeryparameters),
    polarityPrinter (_polarity imagequeryparameters)]

consumeResults :: (MonadIO m) => FilePath -> Handle -> Consumer ImageQueryResult m r
consumeResults resultsPath tablehandle = flip evalStateT (0,Nothing,Nothing) (forever (do

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
        saveIslandImages resultsPath n (_outputImages imagequeryresult)
        saveTableRow tablehandle (_tableRow imagequeryresult)
        forM_ (finalizeAverageImage maybeaverageimage' n') (saveAverageImage resultsPath)
        forM_ maybelineimages' (saveLineImages resultsPath)
        saveHistograms resultsPath n (_histograms imagequeryresult))))

saveIslandImages :: FilePath -> Int -> [(ImageQueryParameters,Image Pixel8)] -> IO ()
saveIslandImages resultsPath n islandimages = forM_ islandimages (\(imagequeryparameters,islandimage) -> do
    let islandimagepath =
            resultsPath </> islandImagesPath </>
            parametersPath imagequeryparameters </> islandImagePath n
    writeBitmap islandimagepath (imageToJuicy islandimage)) where
        
saveHistograms :: FilePath -> Int -> [(ImageQueryParameters,Int,Power,[(Int,Int)])] -> IO ()
saveHistograms resultsPath i histograms = forM_ histograms (\(imagequeryparameters,binsize,power,histogram) -> do
    let histogramRow r v = show r ++ " " ++ show v
        histogrampath =
            resultsPath </> histogramsPath </>
            parametersPath imagequeryparameters </> histogramPath i
    writeFile histogrampath (unlines (map (uncurry histogramRow) histogram)))

saveAverageImage :: FilePath -> Image Pixel8 -> IO ()
saveAverageImage resultsPath = writeBitmap (resultsPath </> averageImagePath) . imageToJuicy

saveLineImages :: FilePath -> [Image Pixel8] -> IO ()
saveLineImages resultsPath lineimages = forM_ (zip [0..] lineimages) (\(i,lineimage) -> do
    writeBitmap (resultsPath </> lineImagePath i) (imageToJuicy lineimage))

saveTableRow :: Handle -> [Double] -> IO ()
saveTableRow tablehandle tablerow = hPutStrLn tablehandle (csvRow tablerow)

csvRow :: [Double] -> String
csvRow = unwords . map show

onFailure :: (Monad m) => EitherT a m b -> (a -> c) -> EitherT c m b
onFailure = flip fmapLT
