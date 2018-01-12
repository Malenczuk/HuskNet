module Mnist (Image(..),readTrainingData, readTestData, getData)
  where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import qualified Data.List.Split as S
import Numeric.LinearAlgebra

data Image = Image {
      iRows :: Int
    , iColumns :: Int
    , iPixels :: [Word8]
    } deriving (Eq, Show)

type LabelledImage = (Matrix Double, Int)

toMatrix :: Image -> Matrix Double
toMatrix image = (r><c) p :: Matrix Double
  where r = iRows image
        c = iColumns image
        p = fromIntegral <$> iPixels image

toVector :: Image -> Matrix Double
toVector image = (rc><1) p :: Matrix Double
  where rc = iRows image * iColumns image
        p = normalisedData image

normalisedData :: Image -> [Double]
normalisedData image = normalisePixel <$> iPixels image

normalisePixel :: Word8 -> Double
normalisePixel p = fromIntegral p / 255.0

{-
[offset] [type]          [value]          [description]
0000     32 bit integer  0x00000801(2049) magic number (MSB first)
0004     32 bit integer  60000            number of items
0008     unsigned byte   ??               label
0009     unsigned byte   ??               label
 ........
xxxx     unsigned byte   ??               label
The labels values are 0 to 9.
-}

deserialiseLabels :: Get (Word32, Word32, [Word8])
deserialiseLabels = do
  magicNumber <- getWord32be
  count <- getWord32be
  labelData <- getRemainingLazyByteString
  let labels = BL.unpack labelData
  return (magicNumber, count, labels)

readLabels :: FilePath -> IO [Int]
readLabels filename = do
  content <- GZip.decompress <$> BL.readFile filename
  let (_, _, labels) = runGet deserialiseLabels content
  return (fromIntegral <$> labels)

{-
[offset] [type]          [value]          [description]
0000     32 bit integer  0x00000803(2051) magic number
0004     32 bit integer  60000            number of images
0008     32 bit integer  28               number of rows
0012     32 bit integer  28               number of columns
0016     unsigned byte   ??               pixel
0017     unsigned byte   ??               pixel
 ........
xxxx     unsigned byte   ??               pixel
Pixels are organized row-wise. Pixel values are 0 to 255. 0 means background (white), 255 means foreground (black).
-}

deserialiseHeader :: Get (Word32, Word32, Word32, Word32, [[Word8]])
deserialiseHeader = do
  magicNumber <- getWord32be
  imageCount <- getWord32be
  r <- getWord32be
  c <- getWord32be
  packedData <- getRemainingLazyByteString
  let len = fromIntegral (r * c)
  let unpackedData = S.chunksOf len (BL.unpack packedData)
  return (magicNumber, imageCount, r, c, unpackedData)

readImages :: FilePath -> IO [Image]
readImages filename = do
  content <- GZip.decompress <$> BL.readFile filename
  let (_, _, r, c, unpackedData) = runGet deserialiseHeader content
  return (Image (fromIntegral r) (fromIntegral c) <$> unpackedData)

vectorizedLabels :: Int -> Matrix Double
vectorizedLabels x = (10><1) p :: Matrix Double
  where p = [i | j <- [0 .. 9], let i = if j == x then 1.0 else 0.0]

readTrainingData :: IO [(Matrix Double, Matrix Double)]
readTrainingData = do
  trainingLabels <- readLabels "train-labels-idx1-ubyte.gz"
  trainingImages <- readImages "train-images-idx3-ubyte.gz"
  return (zip (map toVector trainingImages) (vectorizedLabels <$> trainingLabels))

readTestData :: IO [(Matrix Double, Int)]--IO [LabelledImage]
readTestData = do
  testLabels <- readLabels "t10k-labels-idx1-ubyte.gz"
  testImages <- readImages "t10k-images-idx3-ubyte.gz"
  return (zip (map toVector testImages) testLabels)

getData :: IO ([(Matrix Double, Matrix Double)], [(Matrix Double, Int)])
getData = do
  trainingData <- readTrainingData
  testData <- readTestData
  return (trainingData, testData)