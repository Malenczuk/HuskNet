module Main where

import Specs
import Mnist
import GradientDescent
import Interface
import Network
import Numeric.LinearAlgebra




main :: IO ()
main = do
    numHiddenLayers <- getLayersNumber
    hiddenSizes <- getSizes numHiddenLayers
    net <- createNet ([28 * 28] ++ hiddenSizes ++ [10])
--    net <- loadNetwork "14.txt"
    epochs <- getEpochs
    (trainingData, testData) <- getData
    let (trainingData2, testData2) = (take 60000 trainingData, take 10000 testData)

    newNet <- gradientDescent net trainingData2 epochs 10 1.0 testData2
    let evaluatedData = getOutcomes newNet testData2
--    saveNetwork newNet "15.txt"
    interface evaluatedData
