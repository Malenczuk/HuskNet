{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module GradientDescent (getOutcomes, sgd) where

import Numeric.LinearAlgebra
import qualified Data.List.Split as S
import System.Random
import Data.Array.IO
import Control.Monad
import GHC.Float
import Network


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

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1.0 + exp (-x))

matrixSigmoid :: Matrix Double -> Matrix Double
matrixSigmoid = cmap sigmoid

sigmoidPrime :: Double -> Double
sigmoidPrime x = sigmoid x * (1 - sigmoid x)

matrixSigmoidPrime :: Matrix Double -> Matrix Double
matrixSigmoidPrime = cmap sigmoidPrime

costPrime :: Matrix Double -> Matrix Double -> Matrix Double
costPrime outputActivations y = outputActivations - y

feedForward :: Net -> Matrix Double -> Matrix Double
feedForward net x = foldl (\z (w,b)-> matrixSigmoid ((w <> z) + b)) x (zip (weights net) (biases net))

evaluate :: Net -> [(Matrix Double, Int)] -> Double
evaluate net testData = sum $ fmap (\(x,y)->if x==y then 1.0 else 0.0) outcomes
  where
    testImages = map fst testData
    testLabels = map snd testData
    outcomes = zip (fmap (head . fmap maxIndex . toColumns . feedForward net) testImages) testLabels

getOutcomes :: Net -> [(Matrix Double, Int)] -> [([Float], (Int, Int))]
getOutcomes net testData = zip listImages outcomes
  where
    testImages = map fst testData
    testLabels = map snd testData
    listImages = head . toLists . tr . cmap double2Float <$> testImages
    outcomes = zip (fmap (head . fmap maxIndex . toColumns . feedForward net) testImages) testLabels

neuronActivation :: [Matrix Double] -> [Matrix Double] -> Matrix Double -> [Matrix Double]
neuronActivation [] [] x = []
neuronActivation (w:ws) (b:bs) x = z : neuronActivation ws bs (matrixSigmoid z)
  where
    z = (w <> x) + b

backingUp :: Net -> [Matrix Double] -> Matrix Double -> Int -> [Matrix Double]
backingUp net zs delta l | l == numLayers net = []
                         | otherwise = d : backingUp net zs d (l + 1)
                            where
                              z = reverse zs !! (l-1)
                              sp = matrixSigmoidPrime z
                              w = reverse (weights net) !! (l - 2)
                              d = tr w <> delta * sp


backProp :: Net -> Matrix Double -> Matrix Double -> ([Matrix Double],[Matrix Double])
backProp net x y = (nablaW, nablaB)
  where
    zs = neuronActivation (weights net) (biases net) x
    activations = x : (matrixSigmoid <$> zs)
    delta = (costPrime (last activations) y * matrixSigmoidPrime (last zs)) :: Matrix Double
    lDelta = reverse (delta : backingUp net zs delta 2)
    nablaB = lDelta
    nablaW = (\ l -> (lDelta !! l) <> tr (activations !! l) ) <$> [0..(numLayers net - 2)]


calNabla :: Net -> [(Matrix Double, Matrix Double)] -> [Matrix Double] -> [Matrix Double] -> ([Matrix Double], [Matrix Double])
calNabla net [] nW nB = (nW, nB)
calNabla net (x:xs) nW nB = calNabla net xs nablaW nablaB
  where
    (deltaNablaW, deltaNablaB) = uncurry (backProp net) x
    nablaB = uncurry (+) <$> zip nB deltaNablaB
    nablaW = uncurry (+) <$> zip nW deltaNablaW


updateMiniBatch :: Net -> [(Matrix Double, Matrix Double)] -> Double -> Net
updateMiniBatch net miniBatch alpha = newNet
  where
    nB = fmap (\x -> konst (0 :: Double) ((fst $ size x) :: Int, (snd $ size x) :: Int) :: Matrix Double) (biases net)
    nW = fmap (\x -> konst (0 :: Double) ((fst $ size x) :: Int, (snd $ size x) :: Int) :: Matrix Double) (weights net)
    (nablaW, nablaB) = calNabla net miniBatch nW nB
    newWeights = (\(x,y) -> x - cmap (\ p -> (alpha / fromIntegral (length miniBatch)) * p) y ) <$> zip (weights net) nablaW
    newBiases = (\(x,y) -> x - cmap (\ p -> (alpha / fromIntegral (length miniBatch)) * p) y ) <$> zip (biases net) nablaB
    newNet = Net (numLayers net) (sizes net) newBiases newWeights

sgd :: Net -> [(Matrix Double, Matrix Double)] -> Int -> Int -> Int -> Double -> [(Matrix Double, Int)] -> IO Net
sgd net trainingData epochs epoch miniBatchSize alpha testData = do
    let nTest = fromIntegral (length testData)
    if epoch == epochs then do
      print ("Epoch: " ++ show epoch ++ " = " ++ show (100.0 * evaluate net testData / nTest) ++ "% Out Of " ++ show nTest ++ " Test Images")
      return net
    else do
      print("Epoch: " ++ show epoch ++ " = " ++ show (100.0 * evaluate net testData / nTest) ++ "% Out Of " ++ show nTest ++ " Test Images")
      shuffledTrainingData <- shuffle trainingData
      let miniBatches = S.chunksOf miniBatchSize shuffledTrainingData
      let newNet = foldl (\z x-> updateMiniBatch z x alpha) net miniBatches
      sgd newNet trainingData epochs (epoch + 1) miniBatchSize alpha testData

