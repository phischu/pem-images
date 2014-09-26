{-# LANGUAGE DeriveDataTypeable #-}
module ImageLoading where

import ImageProcessing (Image,RGB,juicyToImage)

import Pipes
import qualified Pipes.Prelude as Pipes

import qualified Codec.Picture as Juicy (Image,PixelRGB8)
import Codec.Picture (readImage,DynamicImage(ImageRGB8,ImageY8,ImageYCbCr8))
import Codec.Picture.Types (promoteImage,convertImage)

import System.Directory (getDirectoryContents,doesFileExist)
import System.FilePath ((</>))
import Control.Exception (Exception,throw)
import Data.Typeable (Typeable)

imageSeries :: FilePath -> Producer (FilePath,Image RGB) IO ()
imageSeries seriespath =
    filesInDirectory seriespath >->
    Pipes.mapM (\imagepath -> do
        image <- loadImage imagepath
        return (imagepath,juicyToImage image))

data ImageLoadingError =
    ImageLoadingError String String deriving (Typeable)

instance Show ImageLoadingError where
    show (ImageLoadingError imagepath message) = "Error loading " ++ imagepath ++ ":\n" ++ message

instance Exception ImageLoadingError

filesInDirectory :: FilePath -> Producer FilePath IO ()
filesInDirectory directorypath = do
    directorycontents <- liftIO (getDirectoryContents directorypath)
    each directorycontents >-> Pipes.map (directorypath </>) >-> Pipes.filterM (liftIO . doesFileExist)

loadImage :: FilePath -> IO (Juicy.Image Juicy.PixelRGB8)
loadImage imagepath = do
    eitherdynamicimage <- readImage imagepath
    case eitherdynamicimage of
        Left readimageerror -> throw (ImageLoadingError imagepath readimageerror)
        Right (ImageRGB8 image) -> return image
        Right (ImageY8 image) -> return (promoteImage image)
        Right (ImageYCbCr8 image) -> return (convertImage image)
        _ -> throw (ImageLoadingError imagepath "unsupported image format")

