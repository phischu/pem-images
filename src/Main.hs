module Main where

import Pipes
import qualified Pipes.Prelude as Pipes
import Codec.Picture
import Data.Vector
import System.Directory
import System.FilePath

filesInDirectory :: (MonadIO m) => FilePath -> Producer FilePath m ()
filesInDirectory directorypath = do
    directorycontents <- liftIO (getDirectoryContents directorypath)
    each directorycontents >-> Pipes.filterM (liftIO . doesFileExist . (directorypath </>))

loadImage :: FilePath -> IO (Image PixelRGB8)
loadImage = undefined

chooseChannel ::Image PixelRGB8 -> Image Pixel8
chooseChannel = undefined

gatherRowData :: Image Pixel8 -> Vector Double
gatherRowData = undefined

testdirectory :: FilePath
testdirectory = "data/2008-05/PEEM08050800/"

main :: IO ()
main = runEffect (
    filesInDirectory testdirectory >->
    Pipes.print)
