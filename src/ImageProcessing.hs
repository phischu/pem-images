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
    Array,indices,(!))
import qualified Data.Set as Set (empty,insert,size)
import Data.Array.ST (
    STArray,runSTArray,newArray,newArray_,readArray,writeArray)
import Control.Monad.ST (
    ST)
import Data.STRef.Strict (
    newSTRef,readSTRef,writeSTRef)
import qualified Data.UnionFind.ST as UnionFind (
    Point,fresh,equivalent,union,descriptor)

import Control.Monad (when)
import Data.Traversable (forM)

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector (map,enumFromStepN,length)

type Image a = Repa.Array D DIM2 a
data RGB = RGB {
    red :: Word8,
    green :: Word8,
    blue :: Word8}
type Rect = (Int,Int,Int,Int)

juicyToImage :: Juicy.Image Juicy.PixelRGB8 -> Image RGB
juicyToImage juicy = Repa.delay (
    Repa.fromFunction shape (\(Z:.y:.x) -> case Juicy.pixelAt juicy x y of
            Juicy.PixelRGB8 r g b -> RGB r g b)) where
        shape = Z:.h:.w
        w = Juicy.imageWidth juicy
        h = Juicy.imageHeight juicy

imageToJuicy :: Image Word8 -> Juicy.Image Juicy.Pixel8
imageToJuicy image = Juicy.generateImage (\x y -> index image (Z:.y:.x)) w h where
    Z:.h:.w = extent image

cutOut :: Rect -> Image Bool -> Image Bool
cutOut (x,y,w,h) image = Repa.backpermuteDft (Repa.fromFunction shape (const False)) indexInShape image where
    shape = Z:.h:.w
    indexInShape (Z:.y':.x') = if inShape (extent image) (Z:.y'+y:.x'+x) then Just (Z:.y'+y:.x'+x) else Nothing

identityStencil :: Int -> Int -> Image Bool
identityStencil width height = Repa.fromFunction (Z:.height:.width) (const True)

applyStencil :: Image Bool -> Image Bool -> Image Bool
applyStencil stencil image = Repa.delay (Repa.computeUnboxedS (Repa.zipWith (&&) stencil' image)) where
    shape = extent image
    stencil' = Repa.backpermuteDft (Repa.fromFunction shape (const False)) indexInShape stencil
    indexInShape position = if inShape shape position then Just position else Nothing

invert :: Image Bool -> Image Bool
invert image = Repa.map not image 

chooseChannel :: (RGB -> Word8) -> Image RGB -> Image Word8
chooseChannel channel = Repa.delay . Repa.computeUnboxedS . Repa.map channel

type Threshold = Word8

smooth :: Int -> Image Word8 -> Image Word8
smooth 0 = id
smooth radius = Repa.map (fromIntegral . (`div` size)) . Repa.map sumPixels . neighbourhood radius where
    size = width * width
    width = 2 * fromIntegral radius + 1

neighbourhood :: Int -> Image Word8 -> Image (Image Word8)
neighbourhood r image = Repa.traverse image id f where
    f a (Z:.y:.x) = Repa.fromFunction (Z:.2*r+1:.2*r+1) (\(Z:.j:.i) -> a (Z :. y + r - j :. x + r - i))

sumPixels :: Image Word8 -> Int
sumPixels = Repa.sumAllS . Repa.map fromIntegral

valueInPoint :: (Num a) => Int -> Int -> Image a -> a
valueInPoint x y image = withDefault (extent image) 0 (index image) (Z:.y:.x)

averageAroundPoint :: (Num a,Integral a,Num b,Fractional b) => Int -> Int -> Int -> Image a -> b
averageAroundPoint x y r image = sum pixelvalues / (2 * fromIntegral r + 1)^two where
    pixelvalues = do
        dx <- [-r..r]
        dy <- [-r..r]
        return (fromIntegral (valueInPoint (x+dx) (y+dy) image))
    two = 2 :: Int

averageOfImage :: (Integral a) => Image a -> Double
averageOfImage image = sumOfPixels / numberOfPixels where
    sumOfPixels = Repa.sumAllS (Repa.map fromIntegral image :: Image Double)
    numberOfPixels = fromIntegral (w * h)
    Z:.h:.w = extent image

numberOfIslands :: Image Bool -> Int
numberOfIslands image = numberOfLabels (labelArray image)

binarize :: Threshold -> Image Word8 -> Image Bool
binarize threshold image = Repa.delay (Repa.computeUnboxedS (Repa.map (\pixelvalue -> pixelvalue > threshold) image))

blackAndWhite :: Image Bool -> Image Word8
blackAndWhite = Repa.delay . Repa.computeUnboxedS . Repa.map (\b -> if b then 255 else 0)

numberOfTruePixels :: Image Bool -> Double
numberOfTruePixels image = Repa.sumAllS (Repa.map boolToDouble image) where
    boolToDouble False = 0.0
    boolToDouble True = 1.0

numberOfOutlinePixels :: Image Bool -> Double
numberOfOutlinePixels image = numberOfTruePixels (Repa.traverse image id isOutline) where
    isOutline i (Z:.y:.x) = i (Z:.y:.x) && not (all (withDefault (extent image) False i) neighbours) where
        neighbours = [Z:.y:.x-1,Z:.y:.x+1,Z:.y-1:.x,Z:.y+1:.x]

withDefault :: (Shape sh) => sh -> a -> (sh -> a) -> sh -> a
withDefault shape def image position
    | inShape shape position = image position
    | otherwise = def

horizontalLine :: Int -> Int -> Int -> Image Word8 -> Vector Word8
horizontalLine fromx fromy pixelsonline image =
    Vector.map (\x -> valueInPoint x fromy image)
        (Vector.enumFromStepN fromx step n) where
            step = signum pixelsonline
            n = abs pixelsonline

verticalLine :: Int -> Int -> Int -> Image Word8 -> Vector Word8
verticalLine fromx fromy pixelsonline image =
    Vector.map (\y -> valueInPoint fromx y image)
        (Vector.enumFromStepN fromy step n) where
            step = signum pixelsonline
            n = abs pixelsonline


singleLineImage :: Vector Word8 -> Image Word8
singleLineImage line = Repa.delay (Repa.fromUnboxed (Z:.h:.1) line)where
    h = Vector.length line

appendLine :: Image Word8 -> Vector Word8 -> Image Word8
appendLine image line = Repa.append image (Repa.fromUnboxed (Z:.h:.1) line) where
    (Z:.h:._) = extent image


singleAverageImage :: Image Word8 -> Image Integer
singleAverageImage image = Repa.map fromIntegral image

addImage :: Image Integer -> Image Word8 -> Image Integer
addImage accuImage image = accuImage +^ (Repa.map fromIntegral image)

finalizeAverageImage :: (Maybe (Image Integer)) -> Int -> Maybe (Image Word8)
finalizeAverageImage Nothing _ = Nothing
finalizeAverageImage (Just image) n
    | n <= 0 = Nothing
    | otherwise = Just (Repa.map (\pixelvalue -> fromIntegral (pixelvalue `div` fromIntegral n)) image)


areaHistogram :: Int -> Int -> (Int -> Int) -> Image Bool -> Array Int Int
areaHistogram bins _ _ _ = runSTArray (do
    newArray_ (0,bins - 1))

labelArray :: Image Bool -> Array (Int,Int) Int
labelArray image = runSTArray (do
    currentlabelref <- newSTRef 1
    let Z:.h:.w = extent image
        lastx = w - 1
        lasty = h - 1
    zero <- UnionFind.fresh 0
    pointimage <- newArray_ ((0,0),(lastx,lasty)) :: ST s (STArray s (Int,Int) (UnionFind.Point s Int))
    forM [0..lasty] (\y -> do
        forM [0..lastx] (\x -> do
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
    forM [0..lasty] (\y -> do
        forM [0..lastx] (\x -> do
            point <- readArray pointimage (x,y)
            label <- UnionFind.descriptor point
            writeArray labelimage (x,y) label))     
    return labelimage)

numberOfLabels :: Array (Int,Int) Int -> Int
numberOfLabels arr = Set.size (go Set.empty (indices arr)) where
    go !labels [] = labels
    go !labels (i:rest)
        | (arr ! i) == 0 = go labels rest
        | otherwise = go (Set.insert (arr ! i) labels) rest
