module ImageQuery where

import ImageProcessing (
    Image,Rect,Threshold,RGB(red,green,blue),
    valueInPoint,averageAroundPoint,averageOfImage,
    cutOut,binarize,applyStencil,invert,blackAndWhite,chooseChannel,smooth,
    numberOfIslands,numberOfTruePixels,numberOfOutlinePixels,
    horizontalLine,verticalLine)

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
    IslandImage Polarity |
    AreaHistogram Polarity Int Int Power deriving Show

data ImageQueryParameter =
    Channel Channel |
    SubRect Rect |
    StencilImage FilePath (Maybe (Image Bool)) |
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

data Channel =
    Red |
    Green |
    Blue deriving Show

data Power =
    One |
    OneOverTwo |
    ThreeOverTwo deriving Show

data ImageQueryParameters = ImageQueryParameters {
    _channel :: Channel,
    _subRect :: Maybe Rect,
    _stencilImage :: Maybe (Image Bool),
    _threshold :: Threshold,
    _smoothing :: Int}

data ImageQueryOutput =
    OutputImage (Image Word8) |
    AverageImage (Image Word8) |
    TableValue Double |
    ImageLine (Unboxed.Vector Word8) |
    Histogram Int (Unboxed.Vector Int)

data ImageQueryResult = ImageQueryResult {
    _outputImages :: [Image Word8],
    _averageImages :: [Image Word8],
    _tableRow :: [Double],
    _imageLines :: [Unboxed.Vector Word8],
    _histograms :: [(Int,Unboxed.Vector Int)]}

instance Monoid ImageQueryResult where
    mempty = ImageQueryResult [] [] [] [] []
    mappend
        (ImageQueryResult outputimages1 averageimages1 tablerow1 imagelines1 histograms1)
        (ImageQueryResult outputimages2 averageimages2 tablerow2 imagelines2 histograms2) = ImageQueryResult
            (mappend outputimages1 outputimages2)
            (mappend averageimages1 averageimages2)
            (mappend tablerow1 tablerow2)
            (mappend imagelines1 imagelines2)
            (mappend histograms1 histograms2)

initialImageQueryParameters :: ImageQueryParameters
initialImageQueryParameters = ImageQueryParameters Red Nothing Nothing 0 0

outputToResult :: ImageQueryOutput -> ImageQueryResult
outputToResult (OutputImage outputimage) = mempty {_outputImages = [outputimage]}
outputToResult (AverageImage averageimage) = mempty {_averageImages = [averageimage]}
outputToResult (TableValue tablevalue) = mempty {_tableRow = [tablevalue]}
outputToResult (ImageLine imageline) = mempty {_imageLines = [imageline]}
outputToResult (Histogram keys values) = mempty {_histograms = [(keys,values)]}

runImageQueries :: (Monad m) => [ImageQueryStatement] -> Image RGB -> m ImageQueryResult
runImageQueries imagequerystatements image = flip evalStateT initialImageQueryParameters (do
    imagequeryoutputs <- forM imagequerystatements (runImageQuery image)
    return (foldMap outputToResult (catMaybes imagequeryoutputs)))

runImageQuery :: (Monad m) => Image RGB -> ImageQueryStatement -> StateT ImageQueryParameters m (Maybe ImageQueryOutput)
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
setImageQueryParameter (StencilImage _ (Just stencilimage)) =
    modify (\imagequeryparameters -> imagequeryparameters {_stencilImage = Just stencilimage})
setImageQueryParameter (Threshold threshold) =
    modify (\imagequeryparameters -> imagequeryparameters {_threshold = threshold})
setImageQueryParameter (Smoothing smoothing) =
    modify (\imagequeryparameters -> imagequeryparameters {_smoothing = smoothing})

getImageQueryOutput :: Image RGB -> ImageQueryParameters -> ImageQuery -> ImageQueryOutput
getImageQueryOutput image imagequeryparameters imagequery =
    let channel = case _channel imagequeryparameters of
            Red -> red
            Green -> green
            Blue -> blue
        grayimage = chooseChannel channel image
    in case imagequery of
        TableQuery tablequery -> runTableQuery grayimage imagequeryparameters tablequery
        ImageOfAverage -> AverageImage grayimage
        LineImage Horizontal x y l -> ImageLine (horizontalLine x y l grayimage)
        LineImage Vertical x y l -> ImageLine (verticalLine x y l grayimage)
        IslandImage polarity ->
            OutputImage (blackAndWhite (prepareIslandImage polarity imagequeryparameters grayimage))
        AreaHistogram _ _ _ _ ->
            Histogram undefined undefined

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
prepareIslandImage polarity imagequeryparameters grayimage = cutimage where
    smoothedimage = smooth (_smoothing imagequeryparameters) grayimage
    binaryimage = binarize (_threshold imagequeryparameters) smoothedimage
    invertedimage = case polarity of
        Bright -> binaryimage
        Dark -> invert binaryimage
    stenciledimage = case _stencilImage imagequeryparameters of
        Nothing -> invertedimage
        Just stencilimage -> applyStencil stencilimage invertedimage
    cutimage = case _subRect imagequeryparameters of
        Nothing -> stenciledimage
        Just subrect -> cutOut subrect stenciledimage
