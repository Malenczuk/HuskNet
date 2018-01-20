{-|
Module : Mnist
|-}
module Mnist
  ( Image(..)
  , readTrainingData
  , readTestData
  , getData
  ) where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import qualified Data.List.Split as S
import Numeric.LinearAlgebra


-- | data representing an image
data Image = Image {
      iRows :: Int -- ^ Number of rows
    , iColumns :: Int -- ^ Number of columns
    , iPixels :: [Word8] -- ^ Pixels
    } deriving (Eq, Show)


-- | Create column vector of Image pixels
toColVector :: Image
         -> Matrix Double
toColVector image = (rc><1) p :: Matrix Double
  where rc = iRows image * iColumns image
        p = normalisedData image


-- | Normalise Image pixels
normalisedData :: Image -> [Double]
normalisedData image = normalisePixel <$> iPixels image


-- | Normalise pixel
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


-- | Extract information from labels file
deserialiseLabels :: Get (Word32, Word32, [Word8])
deserialiseLabels = do
  magicNumber <- getWord32be
  count <- getWord32be
  labelData <- getRemainingLazyByteString
  let labels = BL.unpack labelData
  return (magicNumber, count, labels)


-- | Read labels from file
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


-- | Extract information from image file
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


-- | Read Images form file
readImages :: FilePath -> IO [Image]
readImages filename = do
  content <- GZip.decompress <$> BL.readFile filename
  let (_, _, r, c, unpackedData) = runGet deserialiseHeader content
  return (Image (fromIntegral r) (fromIntegral c) <$> unpackedData)


-- | Create column vector of label
vectorizedLabels :: Int -> Matrix Double
vectorizedLabels x = (10><1) p :: Matrix Double
  where p = [i | j <- [0 .. 9], let i = if j == x then 1.0 else 0.0]


-- | Get training data
readTrainingData :: IO [(Matrix Double, Matrix Double)]
readTrainingData = do
  trainingLabels <- readLabels "train-labels-idx1-ubyte.gz"
  trainingImages <- readImages "train-images-idx3-ubyte.gz"
  return (zip (map toColVector trainingImages) (vectorizedLabels <$> trainingLabels))


-- | Get test data
readTestData :: IO [(Matrix Double, Int)]
readTestData = do
  testLabels <- readLabels "t10k-labels-idx1-ubyte.gz"
  testImages <- readImages "t10k-images-idx3-ubyte.gz"
  return (zip (map toColVector testImages) testLabels)


-- | Get training and test data
getData :: IO ([(Matrix Double, Matrix Double)], [(Matrix Double, Int)])
getData = do
  trainingData <- readTrainingData
  testData <- readTestData
  return (trainingData, testData)