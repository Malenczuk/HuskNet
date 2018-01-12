module Network (Net(..), createNet, saveNetwork, loadNetwork) where

import Numeric.LinearAlgebra
import Specs
import System.Random

data Net = Net {
    numLayers :: Int
  , sizes :: [Int]
  , biases :: [Matrix Double]
  , weights :: [Matrix Double]
  } deriving (Eq, Show)

smallRandoms :: Int -> [Double]
smallRandoms seed = map (/100) (randoms (mkStdGen seed))

randomWeightMatrix :: Int -> Int -> Int -> Matrix Double
randomWeightMatrix numInputs numOutputs seed = (numOutputs><numInputs) weights
    where weights = take (numOutputs*numInputs) (smallRandoms seed)


createNet :: [Int] -> IO Net
createNet sizes = do
   let b = (\ x -> randomWeightMatrix 1 x 7) <$> tail sizes
   let w
         = (\ (x, y) -> randomWeightMatrix x y 7) <$>
             zip (init sizes) (tail sizes)
   let net
         = Net{numLayers = length sizes, sizes = sizes,
               biases = b, weights = w}
   return net

toString :: Net -> String
toString net = nL ++ s ++ b ++ w
  where
    nL = show (numLayers net) ++ " \n"
    s = foldr ((\ a b -> a (' ' : b)) . shows) "\n" (sizes net)
    b = foldr1 (++) (foldr ((\ a b -> a (' ' : b)) . shows) "\n" <$> (foldr1 (++) <$> (toLists <$> biases net)))
    w = foldr1 (++) (foldr ((\ a b -> a (' ' : b)) . shows) "\n" <$> (foldr1 (++) <$> (toLists <$> weights net)))



saveNetwork :: Net -> FilePath -> IO ()
saveNetwork net file = writeFile file (toString net)

lineToListDouble :: String -> [Double]
lineToListDouble line = (\ x -> read x :: Double) <$> words line

loadNetwork :: FilePath -> IO Net
loadNetwork file = do
  content <- readFile file
  let contentLines = lines content
  let numLayers = (read (contentLines !! 0) :: Int)
  let sizes = (\x-> read x :: Int) <$> (words (contentLines !! 1))
  let ba = (contentLines !!) <$> [2..numLayers]
  let b = (\ (x, y) -> (y><1) x :: Matrix Double) <$> zip (lineToListDouble <$> ba) (tail sizes)
  let we = (contentLines !!) <$> [(numLayers+1)..(numLayers+numLayers-1)]
  let w  = (\ (x, y, z) -> (z><y) x :: Matrix Double) <$> zip3 (lineToListDouble <$> we) (init sizes) (tail sizes)
  return Net{numLayers = length sizes, sizes = sizes, biases = b, weights = w}