module Main where

import GUI (gui)
import ImageLoading (imageSeries)
import ImageQuery (
    ImageQueryStatement(SetImageQueryParameter,GetImageQueryResult),
    ImageQuery(TableQuery,IslandImage,LineImage,ImageOfAverage),
    ImageQueryParameter(Threshold),
    Orientation(Horizontal),
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

import Pipes (Consumer,runEffect,(>->),await)
import qualified Pipes.Prelude as Pipes (mapM)

import Control.Monad (forever,forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State (StateT,evalStateT,get,put)

import Control.Error (EitherT,runEitherT,scriptIO,hoistEither,fmapLT,left)

import Text.Parsec.String (parseFromFile)

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (Handle,hPutStrLn,openFile,hClose,IOMode(WriteMode))
import System.Environment (getArgs)

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
    GetImageQueryResult ImageOfAverage,
    GetImageQueryResult (LineImage Horizontal 5 6 300)]

differentThresholds :: [ImageQueryStatement]
differentThresholds = do
    threshold <- [0,8..255]
    [SetImageQueryParameter (Threshold threshold),GetImageQueryResult (IslandImage Dark)]

main :: IO ()
main = gui
