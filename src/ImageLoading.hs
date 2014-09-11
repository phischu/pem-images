module ImageLoading where

import ImageProcessing (Image,RGB,juicyToImage)

import Pipes
import qualified Pipes.Prelude as Pipes

import qualified Codec.Picture as Juicy (Image,PixelRGB8)
import Codec.Picture (readImage,DynamicImage(ImageRGB8,ImageY8,ImageYCbCr8))
import Codec.Picture.Types (promoteImage,convertImage)

import System.Directory
import System.FilePath
import Control.Error

imageSeries :: (MonadIO m) => FilePath -> Producer (FilePath,Image RGB) (EitherT ImageLoadingError m) ()
imageSeries seriespath =
    filesInDirectory seriespath >->
    Pipes.mapM (\imagepath -> do
        image <- loadImage imagepath
        return (imagepath,juicyToImage image))

data ImageLoadingError =
    ReadImageError String |
    ImageFormatError deriving (Show)

filesInDirectory :: (MonadIO m) => FilePath -> Producer FilePath m ()
filesInDirectory directorypath = do
    directorycontents <- liftIO (getDirectoryContents directorypath)
    each directorycontents >-> Pipes.map (directorypath </>) >-> Pipes.filterM (liftIO . doesFileExist)

loadImage :: (MonadIO m) => FilePath -> EitherT ImageLoadingError m (Juicy.Image Juicy.PixelRGB8)
loadImage imagepath = do
    eitherdynamicimage <- liftIO (readImage imagepath)
    case eitherdynamicimage of
        Left readimageerror -> left (ReadImageError readimageerror)
        Right (ImageRGB8 image) -> return image
        Right (ImageY8 image) -> return (promoteImage image)
        Right (ImageYCbCr8 image) -> return (convertImage image)
        _ -> left ImageFormatError

