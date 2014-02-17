module Main where

import Pipes
import qualified Pipes.Prelude as Pipes
import Codec.Picture
import Codec.Picture.Types
import Data.Vector
import System.Directory
import System.FilePath
import Control.Error
import qualified Data.Vector as Vector

data ImageError =
    ReadImageError String |
    ImageFormatError deriving (Show)

filesInDirectory :: (MonadIO m) => FilePath -> Producer FilePath m ()
filesInDirectory directorypath = do
    directorycontents <- liftIO (getDirectoryContents directorypath)
    each directorycontents >-> Pipes.filterM (liftIO . doesFileExist . (directorypath </>))

loadImage :: (MonadIO m) => FilePath -> EitherT ImageError m (Image PixelRGB8)
loadImage imagepath = do
    eitherdynamicimage <- liftIO (readImage imagepath)
    case eitherdynamicimage of
        Left readimageerror -> left (ReadImageError readimageerror)
        Right (ImageRGB8 image) -> return image
        _ -> left ImageFormatError

chooseChannel ::Image PixelRGB8 -> Image Pixel8
chooseChannel = extractComponent PlaneRed

gatherRowData :: Image Pixel8 -> Vector Double
gatherRowData image = Vector.fromList [fromIntegral (fromMaybe 0 (mayPixelAt image 20 20))]

mayPixelAt :: (Pixel a) => Image a -> Int -> Int -> Maybe a
mayPixelAt image x y
    | x < 0 || x >= imageWidth image || y < 0 || y > imageHeight image = Nothing
    | otherwise = Just (pixelAt image x y)

testdirectory :: FilePath
testdirectory = "data/2008-05/PEEM08050800/"

main :: IO ()
main = runEffect (
    filesInDirectory testdirectory >->
    Pipes.print)
