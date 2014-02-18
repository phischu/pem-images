module ImageLoading where

import Pipes
import qualified Pipes.Prelude as Pipes
import Codec.Picture
import Codec.Picture.Types
import System.Directory
import System.FilePath
import Control.Error

imageSeries :: (MonadIO m) => FilePath -> Producer (Image Pixel8) (EitherT ImageLoadingError m) ()
imageSeries seriespath =
    filesInDirectory seriespath >->
    Pipes.mapM loadImage

data ImageLoadingError =
    ReadImageError String |
    ImageFormatError deriving (Show)

filesInDirectory :: (MonadIO m) => FilePath -> Producer FilePath m ()
filesInDirectory directorypath = do
    directorycontents <- liftIO (getDirectoryContents directorypath)
    each directorycontents >-> Pipes.map (directorypath </>) >-> Pipes.filterM (liftIO . doesFileExist)

loadImage :: (MonadIO m) => FilePath -> EitherT ImageLoadingError m (Image Pixel8)
loadImage imagepath = do
    eitherdynamicimage <- liftIO (readBitmap imagepath)
    case eitherdynamicimage of
        Left readimageerror -> left (ReadImageError readimageerror)
        Right (ImageRGB8 image) -> return (chooseChannel image)
        Right (ImageY8 image) -> return image
        _ -> left ImageFormatError

chooseChannel ::Image PixelRGB8 -> Image Pixel8
chooseChannel = extractComponent PlaneRed

