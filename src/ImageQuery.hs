module ImageQuery where

import ImageProcessing (
    Image,Rect,Threshold,
    valueInPoint,averageAroundPoint,averageOfImage,
    cutOut,binarize,applyStencil,invert,blackAndWhite,
    numberOfIslands,numberOfTruePixels,numberOfOutlinePixels,
    horizontalLine,verticalLine,toLineImages,
    addImage,finalizeAverageImage)

import Pipes (Producer,yield,for,each)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT,evalStateT,get,modify)
import Data.Monoid (Monoid(..))
import Data.Maybe (catMaybes)
import Control.Monad (forM)
import Data.Foldable (foldMap)

import Data.Word (Word8)

import qualified Data.Vector.Unboxed as Unboxed (Vector)

data ImageQueryStatement =
    SetImageQueryParameter ImageQueryParameter |
    GetImageQueryResult ImageQuery

data ImageQuery =
    TableQuery TableQuery |
    ImageOfAverage |
    LineImage Orientation Int Int Int |
    IslandImage Polarity deriving Show

data ImageQueryParameter =
    Channel Int |
    SubRect Rect |
    StencilImage (Image Bool) |
    Threshold Threshold |
    Smoothing Int

data TableQuery =
    ValueInPoint Int Int |
    AverageAroundPoint Int Int Int |
    AverageOfImage |
    IslandQuery Polarity IslandQuery deriving Show

data IslandQuery =
    NumberOfIslands |
    AverageAreaOfIslands |
    AverageOutlineOfIslands deriving Show

data Orientation =
    Horizontal |
    Vertical deriving Show

data Polarity =
    Dark |
    Bright deriving Show

data ImageQueryParameters = ImageQueryParameters {
    _channel :: Int,
    _subRect :: Maybe Rect,
    _stencilImage :: Maybe (Image Bool),
    _threshold :: Threshold,
    _smoothing :: Int}

data ImageQueryOutput =
    OutputImage (Image Word8) |
    AverageImage (Image Word8) |
    TableValue Double |
    ImageLine (Unboxed.Vector Word8)

data ImageQueryResult = ImageQueryResult {
    _outputImages :: [Image Word8],
    _averageImages :: [Image Word8],
    _tableRow :: [Double],
    _imageLines :: [Unboxed.Vector Word8]}

instance Monoid ImageQueryResult where
    mempty = ImageQueryResult [] [] [] []
    mappend
        (ImageQueryResult outputimages1 averageimages1 tablerow1 imagelines1)
        (ImageQueryResult outputimages2 averageimages2 tablerow2 imagelines2) = ImageQueryResult
            (mappend outputimages1 outputimages2)
            (mappend averageimages1 averageimages2)
            (mappend tablerow1 tablerow2)
            (mappend imagelines1 imagelines2)

initialImageQueryParameters :: ImageQueryParameters
initialImageQueryParameters = ImageQueryParameters 0 Nothing Nothing 0 0

outputToResult :: ImageQueryOutput -> ImageQueryResult
outputToResult (OutputImage outputimage) = mempty {_outputImages = [outputimage]}
outputToResult (AverageImage averageimage) = mempty {_averageImages = [averageimage]}
outputToResult (TableValue tablevalue) = mempty {_tableRow = [tablevalue]}
outputToResult (ImageLine imageline) = mempty {_imageLines = [imageline]}

runImageQueries :: (Monad m) => [ImageQueryStatement] -> Image Word8 -> m ImageQueryResult
runImageQueries imagequerystatements image = flip evalStateT initialImageQueryParameters (do
    imagequeryoutputs <- forM imagequerystatements (runImageQuery image)
    return (foldMap outputToResult (catMaybes imagequeryoutputs)))

runImageQuery :: (Monad m) => Image Word8 -> ImageQueryStatement -> StateT ImageQueryParameters m (Maybe ImageQueryOutput)
runImageQuery _ (SetImageQueryParameter imagequeryparameter) = do
    setImageQueryParameter imagequeryparameter
    return Nothing
runImageQuery image (GetImageQueryResult imagequery) = do
    imagequeryparameters <- get
    return (Just (getImageQueryOutput image imagequeryparameters imagequery))

setImageQueryParameter :: (Monad m) => ImageQueryParameter -> StateT ImageQueryParameters m ()
setImageQueryParameter (Channel channel) =
    modify (\imagequeryparameters -> imagequeryparameters {_channel = channel})
setImageQueryParameter (SubRect (a1,a2,b1,b2)) =
    modify (\imagequeryparameters -> imagequeryparameters {_subRect = Just (a1,a2,b1,b2)})
setImageQueryParameter (StencilImage stencilimage) =
    modify (\imagequeryparameters -> imagequeryparameters {_stencilImage = Just stencilimage})
setImageQueryParameter (Threshold threshold) =
    modify (\imagequeryparameters -> imagequeryparameters {_threshold = threshold})
setImageQueryParameter (Smoothing smoothing) =
    modify (\imagequeryparameters -> imagequeryparameters {_smoothing = smoothing})

getImageQueryOutput :: Image Word8 -> ImageQueryParameters -> ImageQuery -> ImageQueryOutput
getImageQueryOutput image imagequeryparameters (TableQuery tablequery) = runTableQuery image imagequeryparameters tablequery
getImageQueryOutput image _ ImageOfAverage = AverageImage image
getImageQueryOutput image _ (LineImage Horizontal x y l) = ImageLine (horizontalLine x y l image)
getImageQueryOutput image _ (LineImage Vertical x y l) = ImageLine (verticalLine x y l image)
getImageQueryOutput image imagequeryparameters (IslandImage polarity) =
    OutputImage (blackAndWhite (prepareIslandImage polarity imagequeryparameters image))

runTableQuery :: Image Word8 -> ImageQueryParameters -> TableQuery -> ImageQueryOutput
runTableQuery image _ (ValueInPoint x y) = TableValue (fromIntegral (valueInPoint x y image))
runTableQuery image _ (AverageAroundPoint x y r) = TableValue (averageAroundPoint x y r image)
runTableQuery image _ AverageOfImage = TableValue (averageOfImage image)
runTableQuery image imagequeryparameters (IslandQuery polarity islandquery) =
    runIslandQuery (prepareIslandImage polarity imagequeryparameters image) islandquery

runIslandQuery :: Image Bool -> IslandQuery -> ImageQueryOutput
runIslandQuery binaryimage NumberOfIslands = TableValue (fromIntegral (numberOfIslands binaryimage))
runIslandQuery binaryimage AverageAreaOfIslands = TableValue (
    numberOfTruePixels binaryimage / fromIntegral (numberOfIslands binaryimage))
runIslandQuery binaryimage AverageOutlineOfIslands = TableValue (
    numberOfOutlinePixels binaryimage / fromIntegral (numberOfIslands binaryimage))

prepareIslandImage :: Polarity -> ImageQueryParameters -> Image Word8 -> Image Bool
prepareIslandImage polarity imagequeryparameters image = cutimage where
    binaryimage = binarize (_threshold imagequeryparameters) image
    invertedimage = case polarity of
        Bright -> binaryimage
        Dark -> invert binaryimage
    stenciledimage = case _stencilImage imagequeryparameters of
        Nothing -> invertedimage
        Just stencilimage -> applyStencil stencilimage invertedimage
    cutimage = case _subRect imagequeryparameters of
        Nothing -> stenciledimage
        Just subrect -> cutOut subrect stenciledimage
