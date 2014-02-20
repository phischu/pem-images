module Main where

import ImageLoading (imageSeries,ImageLoadingError)
import ImageQuery (ImageQuery(ImageQuery),runImageQuery,ImageQueryAccumulator)

import Codec.Picture (Image,Pixel8)

import Pipes (Producer,runEffect,(>->),hoist,lift)
import qualified Pipes.Prelude as Pipes
import Data.Vector (Vector)
import Data.Vector as V (fromList)
import Control.Error (EitherT,runEitherT)
import Control.Monad.Trans.State (StateT,runStateT)

testdirectory :: FilePath
testdirectory = "data/2008-05/PEEM08050800/"

testquery :: ImageQuery
testquery = undefined

main :: IO ()
main = runStateT undefined (runEitherT (runEffect (
    let produ = hoist (hoist lift) (imageSeries testdirectory) :: Producer (Image Pixel8) (EitherT ImageLoadingError (StateT ImageQueryAccumulator IO)) ()
        consu = hoist lift (runImageQuery testquery) in
    produ >-> consu))) >>= print
