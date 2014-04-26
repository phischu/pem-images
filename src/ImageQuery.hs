module ImageQuery where

import ImageProcessing (
    Image,Rect,Threshold,
    valueInPoint,averageAroundPoint,averageOfImage,
    cutOut,binarize,applyStencil,invert,
    numberOfIslands,numberOfTruePixels,numberOfOutlinePixels,
    horizontalLine,verticalLine,toLineImages,
    addImage,finalizeAverageImage)

import Pipes (Producer,yield,for,each)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT,get,evalStateT)
import Data.Monoid (Monoid(..))
import Data.Maybe (catMaybes)
import Control.Monad (forM)
import Data.Foldable (foldMap)

import Data.Word (Word8)

import qualified Data.Vector.Unboxed as Unboxed (Vector)

data ImageQueryStatement =
    SetImageQueryParameter ImageQueryParameter |
    GetImageQueryResult ImageQuery deriving Show

data ImageQuery =
    TableQuery TableQuery |
    ImageOfAverage |
    LineImage Orientation Int Int Int |
    ThresholdedImage deriving Show

data ImageQueryParameter =
    Channel Int |
    SubRect Int Int Int Int |
    StencilImage String |
    Threshold Word8 |
    Smoothing Int deriving Show

data TableQuery =
    ValueInPoint Int Int |
    AverageAroundPoint Int Int Int |
    AverageOfImage |
    NumberOfIslands Polarity |
    AverageAreaOfIslands Polarity |
    AverageOutlineOfIslands Polarity deriving Show

data Orientation =
    Horizontal |
    Vertical deriving Show

data Polarity =
    Dark |
    Bright deriving Show

data ImageQueryOutput =
    OutputImage (Image Word8) |
    AverageImage (Image Word8) |
    TableValue Double |
    ImageLine (Unboxed.Vector Double)

data ImageQueryParameters = ImageQueryParameters {
    _channel :: Int,
    _subRect :: Maybe (Int,Int,Int,Int),
    _stencilImage :: Maybe String,
    _threshold :: Int,
    _smoothing :: Int}

data ImageQueryResult = ImageQueryResult {
    _outputImages :: [Image Word8],
    _averageImages :: [Image Word8],
    _tableRow :: [Double],
    _imageLines :: [Unboxed.Vector Double]}

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
outputToResult = undefined

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

setImageQueryParameter :: ImageQueryParameter -> StateT ImageQueryParameters m ()
setImageQueryParameter = undefined

getImageQueryOutput :: Image Word8 -> ImageQueryParameters -> ImageQuery -> ImageQueryOutput
getImageQueryOutput = undefined


