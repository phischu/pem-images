{-# LANGUAGE FlexibleInstances,TypeFamilies,BangPatterns #-}
module ImageProcessing where

import qualified Data.Array.Repa as Repa (
    Array,
    sumAllS,map,traverse,delay,zipWith,backpermuteDft,append)
import Data.Array.Repa (
    D,DIM2,extent,
    Shape,inShape,(:.)((:.)),Z(Z),index,
    (+^))
import qualified Data.Array.Repa.Repr.Unboxed as Repa (
    fromUnboxed,computeUnboxedS)
import qualified Data.Array.Repa.Repr.Delayed as Repa (
    fromFunction)

import qualified Codec.Picture as Juicy (
    Image,Pixel8,PixelRGB8(PixelRGB8),pixelAt,imageHeight,imageWidth,generateImage)

import Data.Word (Word8)

import Data.Array (
    Array,elems)
import Data.Map (Map)
import qualified Data.Map.Strict as Map (
    fromListWith,delete,size,elems,toList)
import Data.Array.ST (
    STArray,runSTArray,newArray,newArray_,readArray,writeArray)
import Control.Monad.ST (
    ST)
import Data.STRef.Strict (
    newSTRef,readSTRef,writeSTRef)
import qualified Data.UnionFind.ST as UnionFind (
    Point,fresh,equivalent,union,descriptor)

import Control.Monad (when)
import Control.Loop (numLoop)

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector (map,enumFromStepN,length)

-- | An image is a two dimensional array.
type Image a = Repa.Array D DIM2 a

-- | A triple of red, green and blue values.
data RGB = RGB {
    red :: Word8,
    green :: Word8,
    blue :: Word8}

-- | A rect is a four tuple. The first two values are the coordinates
-- of the top left corner, the other two are the width and height.
type Rect = (Int,Int,Int,Int)

-- | A unique label for an island.
type Label = Int

-- | Convert a freshly loaded image to our image representation.
juicyToImage :: Juicy.Image Juicy.PixelRGB8 -> Image RGB
juicyToImage juicy = Repa.delay (
    Repa.fromFunction shape (\(Z:.y:.x) -> case Juicy.pixelAt juicy x y of
            Juicy.PixelRGB8 r g b -> RGB r g b)) where
        shape = Z:.h:.w
        w = Juicy.imageWidth juicy
        h = Juicy.imageHeight juicy

-- | Convert our image representation to one that can easily be saved.
imageToJuicy :: Image Word8 -> Juicy.Image Juicy.Pixel8
imageToJuicy image = Juicy.generateImage (\x y -> index image (Z:.y:.x)) w h where
    Z:.h:.w = extent image

-- | Cut out the given rect from the given image.
cutOut :: Rect -> Image Bool -> Image Bool
cutOut (x,y,w,h) image = Repa.backpermuteDft (Repa.fromFunction shape (const False)) indexInShape image where
    shape = Z:.h:.w
    indexInShape (Z:.y':.x') = if inShape (extent image) (Z:.y'+y:.x'+x) then Just (Z:.y'+y:.x'+x) else Nothing

-- | A stencil that keeps an entire image of the given width and height.
identityStencil :: Int -> Int -> Image Bool
identityStencil width height = Repa.fromFunction (Z:.height:.width) (const True)

-- | Apply a stencil image to the given image.
applyStencil :: Image Bool -> Image Bool -> Image Bool
applyStencil stencil image = Repa.delay (Repa.computeUnboxedS (Repa.zipWith (&&) stencil' image)) where
    shape = extent image
    stencil' = Repa.backpermuteDft (Repa.fromFunction shape (const False)) indexInShape stencil
    indexInShape position = if inShape shape position then Just position else Nothing

-- | Invert the given image.
invert :: Image Bool -> Image Bool
invert image = Repa.map not image 

-- | Choose the color according to the given choice function.
chooseChannel :: (RGB -> Word8) -> Image RGB -> Image Word8
chooseChannel channel = Repa.delay . Repa.computeUnboxedS . Repa.map channel

-- | A threshold value.
type Threshold = Word8

-- | Apply smoothing with the given radius to the given image.
smooth :: Int -> Image Word8 -> Image Word8
smooth 0 = id
smooth radius = Repa.map (fromIntegral . (`div` size)) . Repa.map sumPixels . neighbourhood radius where
    size = width * width
    width = 2 * fromIntegral radius + 1

-- | Get the neighbourhood of the given size at each pixel position.
neighbourhood :: Int -> Image Word8 -> Image (Image Word8)
neighbourhood r image = Repa.traverse image id f where
    f a (Z:.y:.x) = Repa.fromFunction (Z:.2*r+1:.2*r+1) (\(Z:.j:.i) -> a (Z :. y + r - j :. x + r - i))

-- | Take the sum of all pixel in the given image.
sumPixels :: Image Word8 -> Int
sumPixels = Repa.sumAllS . Repa.map fromIntegral

-- | Get the value at the given pixel position.
valueInPoint :: (Num a) => Int -> Int -> Image a -> a
valueInPoint x y image = withDefault (extent image) 0 (index image) (Z:.y:.x)

-- | Get the average of all pixels around the given point within a square
-- of the given *radius*.
averageAroundPoint :: (Num a,Integral a,Num b,Fractional b) => Int -> Int -> Int -> Image a -> b
averageAroundPoint x y r image = sum pixelvalues / (2 * fromIntegral r + 1)^two where
    pixelvalues = do
        dx <- [-r..r]
        dy <- [-r..r]
        return (fromIntegral (valueInPoint (x+dx) (y+dy) image))
    two = 2 :: Int

-- | The average of all pixel values in the given image.
averageOfImage :: (Integral a) => Image a -> Double
averageOfImage image = sumOfPixels / numberOfPixels where
    sumOfPixels = Repa.sumAllS (Repa.map fromIntegral image :: Image Double)
    numberOfPixels = fromIntegral (w * h)
    Z:.h:.w = extent image

-- | The number of islands in the given binary image.
numberOfIslands :: Image Bool -> Int
numberOfIslands image = numberOfLabels (labelMap (labelArray image))

-- | Apply the given threshold to the given image.
binarize :: Threshold -> Image Word8 -> Image Bool
binarize threshold image = Repa.delay (Repa.computeUnboxedS (Repa.map (\pixelvalue -> pixelvalue > threshold) image))

-- | Convert the given binary image to a gray image.
blackAndWhite :: Image Bool -> Image Word8
blackAndWhite = Repa.delay . Repa.computeUnboxedS . Repa.map (\b -> if b then 255 else 0)

-- | Number of pixels that are 'True'.
numberOfTruePixels :: Image Bool -> Double
numberOfTruePixels image = Repa.sumAllS (Repa.map boolToDouble image) where
    boolToDouble False = 0.0
    boolToDouble True = 1.0

-- | Number of pixels that are on an outline. A pixel is on an outline if
-- it is 'True' and not all its neighbours are 'True'.
numberOfOutlinePixels :: Image Bool -> Double
numberOfOutlinePixels image = numberOfTruePixels (Repa.traverse image id isOutline) where
    isOutline i (Z:.y:.x) = i (Z:.y:.x) && not (all (withDefault (extent image) False i) neighbours) where
        neighbours = [Z:.y:.x-1,Z:.y:.x+1,Z:.y-1:.x,Z:.y+1:.x]

-- | Add a default value for when an index is out of the image.
withDefault :: (Shape sh) => sh -> a -> (sh -> a) -> sh -> a
withDefault shape def image position
    | inShape shape position = image position
    | otherwise = def

-- | Extract a horizontal line.
horizontalLine :: Int -> Int -> Int -> Image Word8 -> Vector Word8
horizontalLine fromx fromy pixelsonline image =
    Vector.map (\x -> valueInPoint x fromy image)
        (Vector.enumFromStepN fromx step n) where
            step = signum pixelsonline
            n = abs pixelsonline

-- | Extract a vertical line.
verticalLine :: Int -> Int -> Int -> Image Word8 -> Vector Word8
verticalLine fromx fromy pixelsonline image =
    Vector.map (\y -> valueInPoint fromx y image)
        (Vector.enumFromStepN fromy step n) where
            step = signum pixelsonline
            n = abs pixelsonline

-- | A line image with only a single line.
singleLineImage :: Vector Word8 -> Image Word8
singleLineImage line = Repa.delay (Repa.fromUnboxed (Z:.h:.1) line)where
    h = Vector.length line

-- | Append the given image and the given line.
appendLine :: Image Word8 -> Vector Word8 -> Image Word8
appendLine image line = Repa.append image (Repa.fromUnboxed (Z:.h:.1) line) where
    (Z:.h:._) = extent image

-- | An average image for a single image.
singleAverageImage :: Image Word8 -> Image Integer
singleAverageImage image = Repa.map fromIntegral image

-- | Add the given average image and the given image.
addImage :: Image Integer -> Image Word8 -> Image Integer
addImage accuImage image = accuImage +^ (Repa.map fromIntegral image)

-- | Divide every value in the maybe given image by the given number.
finalizeAverageImage :: (Maybe (Image Integer)) -> Int -> Maybe (Image Word8)
finalizeAverageImage Nothing _ = Nothing
finalizeAverageImage (Just image) n
    | n <= 0 = Nothing
    | otherwise = Just (Repa.map (\pixelvalue -> fromIntegral (pixelvalue `div` fromIntegral n)) image)

-- | Get a histogram of the areas of islands in the given image with
-- the given bin size. Apply the given function before binning
areaHistogram :: Int -> (Int -> Int) -> Image Bool -> [(Int,Int)]
areaHistogram binsize powerfunction islandimage = Map.toList histogram where
    histogram = Map.fromListWith (+) (zip (map ((*) binsize) values) (repeat 1))
    values = map binning (Map.elems (labelMap (labelArray islandimage)))
    binning area = powerfunction area `div` binsize

-- | A Map from island label to number of occurences in the given array.
labelMap :: Array (Int,Int) Label -> Map Label Int
labelMap arr = Map.delete 0 (Map.fromListWith (+) (zip (elems arr) (repeat 1)))

-- | Number of labels in the given Map.
numberOfLabels :: Map Label Int -> Int
numberOfLabels = Map.size

-- | Given a binary image, give unique labels to islands.
labelArray :: Image Bool -> Array (Int,Int) Label
labelArray image = runSTArray (do
    currentlabelref <- newSTRef 1
    let Z:.h:.w = extent image
        lastx = w - 1
        lasty = h - 1
    zero <- UnionFind.fresh 0
    pointimage <- newArray_ ((0,0),(lastx,lasty)) :: ST s (STArray s (Int,Int) (UnionFind.Point s Int))
    numLoop 0 lasty (\y -> do
        numLoop 0 lastx (\x -> do
            writeArray pointimage (x,y) zero
            when (index image (Z:.y:.x)) (do
                leftpoint <- if x <= 0 then return zero else readArray pointimage (x-1,y)
                upperpoint <- if y <= 0 then return zero else readArray pointimage (x,y-1)
                leftiszero <- UnionFind.equivalent leftpoint zero
                upperiszero <- UnionFind.equivalent upperpoint zero
                case (leftiszero,upperiszero) of
                    (True,True) -> do
                        currentlabel <- readSTRef currentlabelref
                        point <- UnionFind.fresh currentlabel
                        writeArray pointimage (x,y) point
                        writeSTRef currentlabelref (currentlabel + 1)
                    (True,False) -> do
                        writeArray pointimage (x,y) upperpoint
                    (False,True) -> do
                        writeArray pointimage (x,y) leftpoint
                    (False,False) -> do
                        writeArray pointimage (x,y) leftpoint
                        UnionFind.union leftpoint upperpoint)))
    labelimage <- newArray ((0,0),(lastx,lasty)) 0
    numLoop 0 lasty (\y -> do
        numLoop 0 lastx (\x -> do
            point <- readArray pointimage (x,y)
            label <- UnionFind.descriptor point
            writeArray labelimage (x,y) label))     
    return labelimage)
