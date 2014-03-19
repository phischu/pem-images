module ImageLoading where

import ImageProcessing (Image,juicyToImage)

import Pipes
import qualified Pipes.Prelude as Pipes

import qualified Codec.Picture as Juicy (Image,Pixel8,PixelRGB8)
import Codec.Picture (readBitmap,DynamicImage(ImageRGB8,ImageY8))
import Codec.Picture.Types (extractComponent,PlaneRed(PlaneRed))

import Data.Word (Word8)

import System.Directory
import System.FilePath
import Control.Error

imageSeries :: (MonadIO m) => FilePath -> Producer (Image Word8) (EitherT ImageLoadingError m) ()
imageSeries seriespath =
    filesInDirectory seriespath >->
    Pipes.mapM loadImage >->
    Pipes.map juicyToImage

data ImageLoadingError =
    ReadImageError String |
    ImageFormatError deriving (Show)

filesInDirectory :: (MonadIO m) => FilePath -> Producer FilePath m ()
filesInDirectory directorypath = do
    directorycontents <- liftIO (getDirectoryContents directorypath)
    each directorycontents >-> Pipes.map (directorypath </>) >-> Pipes.filterM (liftIO . doesFileExist)

loadImage :: (MonadIO m) => FilePath -> EitherT ImageLoadingError m (Juicy.Image Juicy.Pixel8)
loadImage imagepath = do
    eitherdynamicimage <- liftIO (readBitmap imagepath)
    case eitherdynamicimage of
        Left readimageerror -> left (ReadImageError readimageerror)
        Right (ImageRGB8 image) -> return (chooseChannel image)
        Right (ImageY8 image) -> return image
        _ -> left ImageFormatError

chooseChannel :: Juicy.Image Juicy.PixelRGB8 -> Juicy.Image Juicy.Pixel8
chooseChannel = extractComponent PlaneRed

