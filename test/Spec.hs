import Numeric.LinearAlgebra
import Test.Tasty
import Test.Tasty.HUnit
import Data.List
import qualified Test.Tasty.QuickCheck as QC


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps, unitTests]

unitTests =
  testGroup
    "Unit tests"
    [checkTransposition, checkSubtraction]

qcProps =
  testGroup
    "(checked by QuickCheck)"
    []

matrix1 = (2><2) [1,2,3,4] :: Matrix Double
matrix2 = (2><2) [1,1,1,1] :: Matrix Double
trMatrix1 = (2><2) [1,3,2,4] :: Matrix Double
subMatrix1 = (2><2) [0,1,2,3] :: Matrix Double
checkTranspositionMsg = "                        |1 2|    |1 3|" ++ "\n    " ++
                        "Transposition of Matrix |3 4| is |2 4|"

checkSubtractionMsg = "|1 2|   |1 1|    |0 1|" ++ "\n    " ++
                      "|3 4| - |1 1| is |2 3|"

checkTransposition =
  testCase checkTranspositionMsg $ assertEqual [] trMatrix1 (tr matrix1)

checkSubtraction =
  testCase checkSubtractionMsg $ assertEqual [] subMatrix1 (matrix1 - matrix2)






