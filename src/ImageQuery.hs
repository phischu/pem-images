module ImageQuery where

import ImageProcessing (
    Image,Rect,Threshold,
    valueInPoint,averageAroundPoint,averageOfImage,
    cutOut,binarize,applyStencil,invert,
    numberOfIslands,numberOfTruePixels,numberOfOutlinePixels,
    horizontalLine,verticalLine,toLineImages,
    addImage,finalizeAverageImage)

import Pipes (Producer,yield,for,each)
import Pipes.Lift (evalStateP)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT,get)

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

data ImageQueryResult =
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

initialImageQueryParameters :: ImageQueryParameters
initialImageQueryParameters = ImageQueryParameters 0 Nothing Nothing 0 0

runImageQueries :: (Monad m) => [ImageQueryStatement] -> Image Word8 -> Producer ImageQueryResult m ()
runImageQueries imagequeries image = evalStateP initialImageQueryParameters (for (each imagequeries) (runImageQuery image))

runImageQuery :: (Monad m) => Image Word8 -> ImageQueryStatement -> Producer ImageQueryResult (StateT ImageQueryParameters m) ()
runImageQuery _ (SetImageQueryParameter imagequeryparameter) = do
    lift (setImageQueryParameter imagequeryparameter)
runImageQuery image (GetImageQueryResult imagequery) = do
    imagequeryparameters <- lift get
    yield (getImageQueryResult image imagequeryparameters imagequery)

setImageQueryParameter :: ImageQueryParameter -> StateT ImageQueryParameters m ()
setImageQueryParameter = undefined

getImageQueryResult :: Image Word8 -> ImageQueryParameters -> ImageQuery -> ImageQueryResult
getImageQueryResult = undefined


