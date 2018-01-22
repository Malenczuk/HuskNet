module PropertiesTests
 ( properties
 ) where

import GradientDescent
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Numeric.LinearAlgebra


properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]


scProps =
  testGroup
    "(checked by SmallCheck)"
    [checkTranspositionSC, checkMatrixSigmoidSC, checkMatrixSigmoidPrimeSC]


checkTranspositionSC = SC.testProperty "matrix == tr . tr matrix" $
                           \list -> ((length list >< 1) (list :: [Double]) :: Matrix Double)
                              == (tr $ tr $ (length list><1) (list :: [Double]) :: Matrix Double)


checkMatrixSigmoidSC = SC.testProperty "check matrixSigmoid" $
                         \list ->
                            (length (list :: [Double])) >=
                              1 SC.==> ((1 >< length list) (sigmoid <$> list) :: Matrix Double)
                                == matrixSigmoid ((1 >< length list) list :: Matrix Double)


checkMatrixSigmoidPrimeSC = SC.testProperty "check matrixSigmoidPrime" $
                         \list ->
                            (length (list :: [Double])) >=
                              1 SC.==> ((1 >< length list) (sigmoidPrime <$> list) :: Matrix Double)
                                == matrixSigmoidPrime ((1 >< length list) list :: Matrix Double)


qcProps =
  testGroup
    "(checked by QuickCheck)"
    [checkTranspositionQC, checkMatrixSigmoidQC, checkMatrixSigmoidPrimeQC]


checkTranspositionQC = QC.testProperty "matrix == tr . tr matrix" $
                           \list -> ((length list >< 1) (list :: [Double]) :: Matrix Double)
                              == (tr $ tr $ (length list><1) (list :: [Double]) :: Matrix Double)


checkMatrixSigmoidQC = QC.testProperty "check matrixSigmoid" $
                         \list ->
                            (length (list :: [Double])) >=
                              1 QC.==> ((1 >< length list) (sigmoid <$> list) :: Matrix Double)
                                == matrixSigmoid ((1 >< length list) list :: Matrix Double)


checkMatrixSigmoidPrimeQC = QC.testProperty "check matrixSigmoidPrime" $
                         \list ->
                            (length (list :: [Double])) >=
                              1 QC.==> ((1 >< length list) (sigmoidPrime <$> list) :: Matrix Double)
                                == matrixSigmoidPrime ((1 >< length list) list :: Matrix Double)


