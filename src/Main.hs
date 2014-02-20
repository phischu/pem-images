module Main where

import ImageLoading (imageSeries,ImageLoadingError)
import ImageQuery (ImageQuery(ImageQuery),runImageQuery,ImageQueryResult(tableRows))

import Codec.Picture (Image,Pixel8)

import Pipes (Producer,runEffect,(>->),hoist,lift)
import Control.Foldl (purely)
import Pipes.Prelude (fold)
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
main = runEitherT (purely fold (runImageQuery testquery) (imageSeries testdirectory)) >>= either print (print . tableRows)
