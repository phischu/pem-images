module ImageQuery where

import ImageProcessing (
    Image,Rect,Threshold,RGB(red,green,blue),
    valueInPoint,averageAroundPoint,averageOfImage,
    cutOut,binarize,applyStencil,invert,blackAndWhite,chooseChannel,smooth,
    numberOfIslands,numberOfTruePixels,numberOfOutlinePixels,
    horizontalLine,verticalLine,
    areaHistogram)

import Control.Monad.Trans.State.Strict (StateT,evalStateT,get,modify)
import Data.Monoid (Monoid(..))
import Data.Maybe (catMaybes,mapMaybe)
import Control.Monad (forM)
import Data.Foldable (foldMap)

import Data.Word (Word8)

import qualified Data.Vector.Unboxed as Unboxed (Vector)

-- | An image query statement.
data ImageQueryStatement =
    SetImageQueryParameter ImageQueryParameter |
    -- ^ Set some parameter for all following statements.
    GetImageQueryResult ImageQuery
    -- ^ Get some information about an image.

-- ^ Get information about and image.
data ImageQuery =
    TableQuery TableQuery |
    -- ^ Add a value to the table.
    ImageOfAverage |
    -- ^ Create an image that is the pixelwise average of all images.
    LineImage Orientation Int Int Int |
    -- ^ Append lines taken from the images to one image.
    IslandImage |
    -- ^ Output the intermediate island image after thresholding.
    AreaHistogram Int Power
    -- ^ Take a histogram of the areas of the islands.
        deriving Show

-- | Set a parameter for all future statements.
data ImageQueryParameter =
    Channel Channel |
    -- ^ Set the chosen color channel.
    SubRect Rect |
    -- ^ Set the cut out rectangle.
    StencilImage FilePath (Maybe (Image Bool)) |
    -- ^ Set the stencil image to be used.
    Threshold Threshold |
    -- ^ Set the applied threshold.
    Smoothing Int |
    -- ^ Set the smoothing mask size.
    Polarity Polarity
    -- ^ Set the polarity to dark or bright.

-- | Get a value for the table.
data TableQuery =
    ValueInPoint Int Int |
    -- ^ Get the value in a point.
    AverageAroundPoint Int Int Int |
    -- ^ Get the average around a point with given radius.
    AverageOfImage |
    -- ^ Get the average value of the entire image.
    IslandQuery IslandQuery
    -- ^ Get an information about the islands.
        deriving Show

-- | Get information about the islands.
data IslandQuery =
    NumberOfIslands |
    -- ^ Get the number of islands.
    AverageAreaOfIslands |
    -- ^ Get the average area of all islands.
    AverageOutlineOfIslands
    -- ^ Get the average outline of all islands.
        deriving Show

-- | The orientation of lines for line images.
data Orientation =
    Horizontal |
    Vertical deriving Show

-- | The polarity of islands we are interested in. Either dark or bright.
data Polarity =
    Dark |
    Bright deriving Show

-- | A color channel.
data Channel =
    Red |
    Green |
    Blue deriving Show

-- | The power we apply to values before putting them into a histogram.
data Power =
    One |
    OneOverTwo |
    ThreeOverTwo deriving Show

-- | Global parameters during executin of image queries.
data ImageQueryParameters = ImageQueryParameters {
    _channel :: Channel,
    _subRect :: Maybe Rect,
    _stencilImage :: Maybe (Image Bool),
    _threshold :: Threshold,
    _smoothing :: Int,
    _polarity :: Polarity}

-- | The output of some statements.
data ImageQueryOutput =
    OutputImage ImageQueryParameters (Image Word8) |
    AverageImage (Image Word8) |
    TableValue Double |
    ImageLine (Unboxed.Vector Word8) |
    Histogram ImageQueryParameters Int Power [(Int,Int)]

-- | A summary of all outputs.
data ImageQueryResult = ImageQueryResult {
    _outputImages :: [(ImageQueryParameters,Image Word8)],
    _averageImages :: [Image Word8],
    _tableRow :: [Double],
    _imageLines :: [Unboxed.Vector Word8],
    _histograms :: [(ImageQueryParameters,Int,Power,[(Int,Int)])]}

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

-- | Initial parameters. Red channel, no subrect, no stencil, threshold 0,
-- smoothing 0 and dark islands are interesting.
initialImageQueryParameters :: ImageQueryParameters
initialImageQueryParameters = ImageQueryParameters Red Nothing Nothing 0 0 Dark

-- | Convert a single output to a summary.
outputToResult :: ImageQueryOutput -> ImageQueryResult
outputToResult (OutputImage imagequeryparameters outputimage) =
    mempty {_outputImages = [(imagequeryparameters,outputimage)]}
outputToResult (AverageImage averageimage) = mempty {_averageImages = [averageimage]}
outputToResult (TableValue tablevalue) = mempty {_tableRow = [tablevalue]}
outputToResult (ImageLine imageline) = mempty {_imageLines = [imageline]}
outputToResult (Histogram imagequeryparameters binsize power histogram) =
    mempty {_histograms = [(imagequeryparameters,binsize,power,histogram)]}

-- | Run the given statements on the given image.
runImageQueries :: (Monad m) => [ImageQueryStatement] -> Image RGB -> m ImageQueryResult
runImageQueries imagequerystatements image = flip evalStateT initialImageQueryParameters (do
    imagequeryoutputs <- forM imagequerystatements (runImageQuery image)
    return (foldMap outputToResult (catMaybes imagequeryoutputs)))

-- | Run a single statement on the given image.
runImageQuery :: (Monad m) => Image RGB -> ImageQueryStatement -> StateT ImageQueryParameters m (Maybe ImageQueryOutput)
runImageQuery _ (SetImageQueryParameter imagequeryparameter) = do
    setImageQueryParameter imagequeryparameter
    return Nothing
runImageQuery image (GetImageQueryResult imagequery) = do
    imagequeryparameters <- get
    return (Just (getImageQueryOutput image imagequeryparameters imagequery))

-- | Set a parameter.
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
setImageQueryParameter (Polarity polarity) =
    modify (\imagequeryparameters -> imagequeryparameters {_polarity = polarity})

-- | Get output.
getImageQueryOutput :: Image RGB -> ImageQueryParameters -> ImageQuery -> ImageQueryOutput
getImageQueryOutput image imagequeryparameters imagequery =
    let (grayimage,islandimage,numberofislands) = prepareImage imagequeryparameters image
    in case imagequery of
        ImageOfAverage ->
            AverageImage grayimage
        LineImage Horizontal x y l ->
            ImageLine (horizontalLine x y l grayimage)
        LineImage Vertical x y l ->
            ImageLine (verticalLine x y l grayimage)
        IslandImage -> 
            OutputImage imagequeryparameters (blackAndWhite islandimage)
        AreaHistogram binsize power ->
            Histogram imagequeryparameters binsize power histogram where
                histogram = areaHistogram binsize (runPowerFunction power) islandimage
        TableQuery (ValueInPoint x y) ->
            TableValue (fromIntegral (valueInPoint x y grayimage))
        TableQuery (AverageAroundPoint x y r) ->
            TableValue (averageAroundPoint x y r grayimage)
        TableQuery AverageOfImage ->
            TableValue (averageOfImage grayimage)
        TableQuery (IslandQuery NumberOfIslands) ->
            TableValue numberofislands
        TableQuery (IslandQuery AverageAreaOfIslands) ->
            TableValue (numberOfTruePixels islandimage / numberofislands)
        TableQuery (IslandQuery AverageOutlineOfIslands) ->
            TableValue (numberOfOutlinePixels islandimage / numberofislands)

-- | Preprocess the given image according to the given parameters.
prepareImage :: ImageQueryParameters -> Image RGB -> (Image Word8,Image Bool,Double)
prepareImage imagequeryparameters image = (grayimage,islandimage,numberofislands) where
    grayimage = chooseChannel (runChannel (_channel imagequeryparameters)) image
    islandimage =
        maybe id cutOut (_subRect imagequeryparameters) (
            maybe id applyStencil (_stencilImage imagequeryparameters) (
                runPolarity (_polarity imagequeryparameters) (
                    binarize (_threshold imagequeryparameters) (
                        smooth (_smoothing imagequeryparameters) grayimage))))
    numberofislands = fromIntegral (numberOfIslands islandimage)

-- | Choose a channel.
runChannel :: Channel -> RGB -> Word8
runChannel Red = red
runChannel Green = green
runChannel Blue = blue

-- | Conditionally invert the image.
runPolarity :: Polarity -> Image Bool -> Image Bool
runPolarity Dark = invert
runPolarity Bright = id        

-- | Take the given power of the given value.
runPowerFunction :: Power -> Int -> Int
runPowerFunction One = id
runPowerFunction OneOverTwo = round . (sqrt :: Double -> Double) . fromIntegral
runPowerFunction ThreeOverTwo = round . (^(3 :: Int)) . (sqrt :: Double -> Double) . fromIntegral

-- | Top row of the table.
tableHeader :: [ImageQueryStatement] -> [String]
tableHeader imagequerystatements = ["image_name"] ++ mapMaybe entryName imagequerystatements

-- | Column name in the table.
entryName :: ImageQueryStatement -> Maybe String
entryName (GetImageQueryResult (TableQuery tablequery)) = Just (case tablequery of
    ValueInPoint x y -> concat ["value_in_point_",show x,"_",show y]
    AverageAroundPoint x y r -> concat ["average_around_point_",show x,"_",show y,"_",show r]
    AverageOfImage -> "average_of_image"
    IslandQuery islandquery -> case islandquery of
        NumberOfIslands -> "number_of_islands"
        AverageAreaOfIslands -> "average_area_of_islands"
        AverageOutlineOfIslands -> "average_outline_of_islands")
entryName _ = Nothing

-- | Apply a function to every occurence of setting a stencil. Used to load the stencils after
-- loading the statements.
forStencil :: [ImageQueryStatement] -> (ImageQueryParameter -> IO ImageQueryParameter) -> IO [ImageQueryStatement]
forStencil imagequerystatements f = forM imagequerystatements (\imagequerystatement -> do
    case imagequerystatement of
        (SetImageQueryParameter imagequeryparameter@(StencilImage _ _)) -> do
            imagequeryparameter' <- f imagequeryparameter
            return (SetImageQueryParameter imagequeryparameter')
        _ -> return imagequerystatement)
