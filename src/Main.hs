module Main where

import ImageLoading (imageSeries)
import ImageQuery (
    ImageQueryStatement(SetImageQueryParameter,GetImageQueryResult),
    ImageQuery(TableQuery),
    ImageQueryParameter(Threshold),
    TableQuery(..),
    IslandQuery(..),
    Polarity(Dark,Bright),
    runImageQueries,ImageQueryResult(..))
import ImageProcessing (imageToJuicy,identityStencil)

import Codec.Picture (writeBitmap)

import Graphics.UI.WX (
    start,frame,set,Prop((:=)),text,visible,on,closing)

import Pipes ((>->))
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

testdirectory :: FilePath
testdirectory = "data/2008-05/testimages/"

testqueries :: [ImageQueryStatement]
testqueries = [
    SetImageQueryParameter (Threshold 14),
    GetImageQueryResult (TableQuery (IslandQuery Bright NumberOfIslands)),
    GetImageQueryResult (TableQuery (IslandQuery Bright  AverageOutlineOfIslands)),
    SetImageQueryParameter (Threshold 251),
    GetImageQueryResult (TableQuery (IslandQuery Dark NumberOfIslands)),
    GetImageQueryResult (TableQuery (AverageAroundPoint 2 126 12))]

saveResult :: (MonadIO m) => ImageQueryResult -> m ()
saveResult = liftIO . print . _tableRow

main :: IO ()
main = start (do
    frame [text := "wxhNotepad"])

runBatch :: IO ()
runBatch = do
    result <- runEitherT (runEffect (for (imageSeries testdirectory) (runImageQueries testqueries >=> saveResult)))
    case result of
        Left err -> print err
        Right () -> print "alright"