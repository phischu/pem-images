module ImageQuery where

import Codec.Picture (Image,Pixel8)

import Data.Vector (Vector)
import Data.Vector as V (map)

data ImageQuery =
    ValueInPoint Int Int

runImageQuery :: ImageQuery -> Image Pixel8 -> Double
runImageQuery (ValueInPoint x y) image = undefined

runImageQueries :: Vector ImageQuery -> Image Pixel8 -> Vector Double
runImageQueries imagequeries image = V.map (flip runImageQuery image) imagequeries
