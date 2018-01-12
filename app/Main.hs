module Main where

import Specs
import Mnist
import GradientDescent
import Interface
import Network
import Numeric.LinearAlgebra




main :: IO ()
main = do
--    numHiddenLayers <- getLayersNumber
--    hiddenSizes <- getSizes numHiddenLayers 1
--    net <- createNet ([28 * 28] ++ hiddenSizes ++ [10])
    net <- loadNetwork "4.txt"
    epochs <- getEpochs
    (trainingData, testData) <- getData
    let (trainingData2, testData2) = (take 60000 trainingData, take 10000 testData)

    newNet <- sgd net trainingData2 epochs 0 10 1.0 testData2
    let evaluatedData = getOutcomes newNet testData2
    saveNetwork newNet "5.txt"
    interface evaluatedData
