module Run where

import ImageLoading (
    imageSeries,filesInDirectory)
import ImageQuery (
    ImageQueryStatement,runImageQueries,
    ImageQueryParameters(_channel,_smoothing,_threshold,_polarity),
    ImageQueryResult(_averageImages,_imageLines,_outputImages,_tableRow,_histograms),
    Power,
    tableHeader)
import ImageProcessing (
    Image,imageToJuicy,
    singleAverageImage,addImage,finalizeAverageImage,
    singleLineImage,appendLine)
import ImageQuery.Printer (channelPrinter,polarityPrinter,powerPrinter)

import Codec.Picture (Pixel8,writeBitmap)

import Pipes (Consumer,runEffect,(>->),await)
import qualified Pipes.Prelude as Pipes (mapM,length)

import Control.Monad (forever,filterM,(>=>))
import Data.Foldable (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State (evalStateT,get,put)

import System.Directory (createDirectoryIfMissing,doesDirectoryExist)
import System.FilePath ((</>),(<.>),takeBaseName,dropFileName)
import System.IO (
    Handle,hPutStrLn,withFile,IOMode(WriteMode),hSetBuffering,BufferMode(LineBuffering))

import Data.Maybe (listToMaybe)
import Data.List (intercalate)

-- | Actually run the given statements on all images in the given filepath
-- reporting progress with the given function.
run :: (Int -> IO ()) -> FilePath -> [ImageQueryStatement] -> IO ()
run progress inputdirectory imagequerystatements = do
    let inputbasename = takeBaseName inputdirectory
    (resultsPath:_) <- filterM (doesDirectoryExist >=> return . not) (do
        let zero = 0 :: Int
            nine = 9 :: Int
        t <- [zero..nine]
        h <- [zero..nine]
        u <- [zero..nine]
        return ("results" </> (inputbasename ++ show t ++ show h ++ show u)))
    createDirectoryIfMissing True resultsPath
    withFile (resultsPath </> "table" <.> "csv") WriteMode (\tablehandle -> do
        hSetBuffering tablehandle LineBuffering
        hPutStrLn tablehandle (csvRow (tableHeader imagequerystatements))
        runEffect (
            imageSeries inputdirectory >->
            Pipes.mapM (\(imagepath,image) -> do
                imagequeryresult <- runImageQueries imagequerystatements image
                return (imagepath,imagequeryresult)) >->
            consumeResults progress resultsPath tablehandle))

-- | Path where the line images for the i'th line image statement are stored.
lineImagePath :: Int -> FilePath
lineImagePath i = "lineimage-" ++ show i ++ ".bmp"

-- | Create a path from the given parameters.
parametersPath :: ImageQueryParameters -> FilePath
parametersPath imagequeryparameters = intercalate "-" [
    channelPrinter (_channel imagequeryparameters),
    show (_smoothing imagequeryparameters),
    show (_threshold imagequeryparameters),
    polarityPrinter (_polarity imagequeryparameters)]

-- | Number of files in the given folder.
numberOfImages :: FilePath -> IO Int
numberOfImages filepath = Pipes.length (filesInDirectory filepath)

-- | Take a progress reporting function, a path to store the results and the handle
-- of an open table file and give a consumer for pairs of paths and results.
consumeResults :: (Int -> IO ()) -> FilePath -> Handle -> Consumer (FilePath,ImageQueryResult) IO r
consumeResults progress resultsPath tablehandle = flip evalStateT (0,Nothing,Nothing) (forever (do

    (imagepath,imagequeryresult) <- lift await
    (n,maybeaverageimage,maybelineimages) <- get

    let imagebasename = takeBaseName imagepath
        maybeaverageimage' = do
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
        saveIslandImages resultsPath imagebasename (_outputImages imagequeryresult)
        saveTableRow imagebasename tablehandle (_tableRow imagequeryresult)
        forM_ (finalizeAverageImage maybeaverageimage' n') (saveAverageImage resultsPath)
        forM_ maybelineimages' (saveLineImages resultsPath)
        saveHistograms resultsPath imagebasename (_histograms imagequeryresult)
        progress n')))

-- | Write the given intermediate island images to the given path using the given
-- original image name.
saveIslandImages :: FilePath -> FilePath -> [(ImageQueryParameters,Image Pixel8)] -> IO ()
saveIslandImages resultsPath imagebasename islandimages = forM_ islandimages (\(imagequeryparameters,islandimage) -> do
    let islandimagepath =
            resultsPath </> "islandimages" </>
            parametersPath imagequeryparameters </>
            imagebasename <.> "bmp"
    createDirectoryIfMissing True (dropFileName islandimagepath)
    writeBitmap islandimagepath (imageToJuicy islandimage)) where

-- | Save the given histograms to the given path using the given original image name.
saveHistograms :: FilePath -> FilePath -> [(ImageQueryParameters,Int,Power,[(Int,Int)])] -> IO ()
saveHistograms resultsPath imagebasename histograms = forM_ histograms (\(imagequeryparameters,binsize,power,histogram) -> do
    let histogramRow r v = csvRow [show r,show v]
        histogrampath =
            resultsPath </> "histograms" </>
            parametersPath imagequeryparameters </>
            intercalate "-" [show binsize,powerPrinter power] </>
            imagebasename <.> "csv"
    createDirectoryIfMissing True (dropFileName histogrampath)
    writeFile histogrampath (unlines (map (uncurry histogramRow) histogram)))

-- | Save an average image to the given path.
saveAverageImage :: FilePath -> Image Pixel8 -> IO ()
saveAverageImage resultsPath = writeBitmap (resultsPath </> "averageimage" <.> "bmp") . imageToJuicy

-- | Save a list of line image to the given path.
saveLineImages :: FilePath -> [Image Pixel8] -> IO ()
saveLineImages resultsPath lineimages = forM_ (zip [0..] lineimages) (\(i,lineimage) -> do
    writeBitmap (resultsPath </> lineImagePath i) (imageToJuicy lineimage))

-- | Write out the given table row to the given handle using the given
-- original image name.
saveTableRow :: FilePath -> Handle -> [Double] -> IO ()
saveTableRow imagebasename tablehandle tablerow = hPutStrLn tablehandle (csvRow entries) where
    entries = [imagebasename] ++ map show tablerow

-- | A single csv row. Tab separated.
csvRow :: [String] -> String
csvRow = intercalate "\t"
