{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module GradientDescent (getOutcomes, gradientDescent) where

import Numeric.LinearAlgebra
import qualified Data.List.Split as S
import System.Random
import Data.Array.IO
import Control.Monad
import GHC.Float
import Network


-- |
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n = newListArray (1, n)


-- | Sigmoid Function
sigmoid :: Double -> Double
sigmoid x = 1.0 / (1.0 + exp (-x))


-- | Apply Sigmoid Function for every element of matrix
matrixSigmoid :: Matrix Double -> Matrix Double
matrixSigmoid = cmap sigmoid


-- | Sigmoid Prime Function
sigmoidPrime :: Double -> Double
sigmoidPrime x = sigmoid x * (1 - sigmoid x)


-- | Apply Sigmoid Prime Function for every element of matrix
matrixSigmoidPrime :: Matrix Double -> Matrix Double
matrixSigmoidPrime = cmap sigmoidPrime


-- | Cost function
costPrime :: Matrix Double -> Matrix Double  -> Matrix Double
costPrime outputActivations y = outputActivations - y


-- | Feed Forward algorithm
feedForward :: Net -- ^ Neural Network
            -> Matrix Double -- ^ Image Matrix
            -> Matrix Double
feedForward net x = foldl (\z (w,b)-> matrixSigmoid ((w <> z) + b)) x (zip (weights net) (biases net))


-- | Evaluate Neural Network predictions
evaluate :: Net -- ^ Neural Network for evaluation
         -> [(Matrix Double, Int)] -- ^ List of Test Images with their corresponding label
         -> Double -- ^ Returns Count of positive predictions
evaluate net testData = sum $ fmap (\(x,y) -> if x==y then 1.0 else 0.0) outcomes
  where
    testImages = map fst testData
    testLabels = map snd testData
    outcomes = zip (fmap (head . fmap maxIndex . toColumns . feedForward net) testImages) testLabels


-- | Get Neural Network predictions
getOutcomes :: Net -- ^ Neural Network
            -> [(Matrix Double, Int)] -- ^ List of Test Images with their corresponding label
            -> [([Float], (Int, Int))] -- ^ Returns List of Test Images with their corresponding label and predictions
getOutcomes net testData = zip listImages outcomes
  where
    testImages = map fst testData
    testLabels = map snd testData
    listImages = head . toLists . tr . cmap double2Float <$> testImages
    outcomes = zip (fmap (head . fmap maxIndex . toColumns . feedForward net) testImages) testLabels


-- | Get List of neuron activations
neuronActivation :: [Matrix Double] -- ^ Weights of Neural Network
                 -> [Matrix Double] -- ^ Biases of Neural Network
                 -> Matrix Double   -- ^ Image Matrix
                 -> [Matrix Double]
neuronActivation [] [] x = []
neuronActivation (w:ws) (b:bs) x = z : neuronActivation ws bs (matrixSigmoid z)
  where
    z = (w <> x) + b


-- | Calculate deltas
calDelta :: Net -- ^ Neural Network
          -> [Matrix Double] -- ^ Weights of Neural Network
          -> [Matrix Double] -- ^ List of Neuron Activations
          -> Matrix Double   -- ^ delta
          -> Int -- ^ current layer
          -> [Matrix Double]
calDelta net w z delta l
  | l == numLayers net = [delta]
  | otherwise = calDelta net (tail w) (tail z) d (l + 1) ++ [delta]
    where
      sp = matrixSigmoidPrime (head z)
      d = tr (head w) <> delta * sp


-- | Back Propagation algorithm
backProp :: Net -- ^ Neural Network
         -> Matrix Double -- ^ Image Matrix
         -> Matrix Double -- ^ Image vectorized label
         -> ([Matrix Double],[Matrix Double])
backProp net x y = (nablaW, nablaB)
  where
    zs = neuronActivation (weights net) (biases net) x
    activations = x : (matrixSigmoid <$> zs)
    delta = (costPrime (last activations) y * matrixSigmoidPrime (last zs)) :: Matrix Double
    lDelta = calDelta net (reverse $ weights net) (tail $ reverse zs) delta 2
    nablaB = lDelta
    nablaW = (\ (x, y) -> x <> tr y) <$> zip lDelta activations


-- | Calculate Nabla for Weights and Biases
calNabla :: Net -- ^ Neural Network
         -> [(Matrix Double, Matrix Double)] -- ^ Batch of Images with their corresponding labels
         -> [Matrix Double] -- ^ List of Nabla Matrix's of Weights
         -> [Matrix Double] -- ^ List of Nabla Matrix's of Biases
         -> ([Matrix Double], [Matrix Double])
calNabla net [] nW nB = (nW, nB)
calNabla net (x:xs) nW nB = calNabla net xs nablaW nablaB
  where
    (deltaNablaW, deltaNablaB) = uncurry (backProp net) x
    nablaB = uncurry (+) <$> zip nB deltaNablaB
    nablaW = uncurry (+) <$> zip nW deltaNablaW


-- | Update network's weights and biases for each batch
updateMiniBatch :: Net -- ^ Neural Network
                -> [(Matrix Double, Matrix Double)] -- ^ Batch of Images with their corresponding labels
                -> Double -- ^ Alpha
                -> Net
updateMiniBatch net miniBatch alpha = newNet
  where
    nB = fmap (\x -> konst (0 :: Double) ((fst $ size x) :: Int, (snd $ size x) :: Int) :: Matrix Double) (biases net)
    nW = fmap (\x -> konst (0 :: Double) ((fst $ size x) :: Int, (snd $ size x) :: Int) :: Matrix Double) (weights net)
    (nablaW, nablaB) = calNabla net miniBatch nW nB
    newWeights = (\(x,y) -> x - cmap (\ p -> (alpha / fromIntegral (length miniBatch)) * p) y ) <$> zip (weights net) nablaW
    newBiases = (\(x,y) -> x - cmap (\ p -> (alpha / fromIntegral (length miniBatch)) * p) y ) <$> zip (biases net) nablaB
    newNet = Net (numLayers net) (sizes net) newBiases newWeights


-- | Train Neural Network
gradientDescent :: Net -- ^ Neural Network to train
                -> [(Matrix Double, Matrix Double)] -- ^ List of Training Images with column vectorized labels
                -> Int -- ^ Number of training iterations
                -> Int -- ^ Current iteration
                -> Int -- ^ Size of Image batches
                -> Double -- ^ Alpha
                -> [(Matrix Double, Int)] -- ^ List of Test Images with Labels
                -> IO Net
gradientDescent net trainingData epochs epoch miniBatchSize alpha testData = do
  let nTest = fromIntegral (length testData)
  let evaluation = 100.0 * evaluate net testData / nTest
  if epoch == epochs then do
    print ("Epoch: " ++ show epoch ++ " = " ++ show evaluation ++ "% Out Of " ++ show nTest ++ " Test Images")
    return net
  else do
    print ("Epoch: " ++ show epoch ++ " = " ++ show evaluation ++ "% Out Of " ++ show nTest ++ " Test Images")
    shuffledTrainingData <- shuffle trainingData
    let miniBatches = S.chunksOf miniBatchSize shuffledTrainingData
    let newNet = foldl (\z x-> updateMiniBatch z x alpha) net miniBatches
    gradientDescent newNet trainingData epochs (epoch + 1) miniBatchSize alpha testData

