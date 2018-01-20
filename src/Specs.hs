{-|
Module : Specs
|-}
module Specs
    ( getLayersNumber
    , getSizes
    , getEpochs
    ) where


-- | Maybe reader
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing


-- | Get number of hidden Layers
getLayersNumber :: IO Int
getLayersNumber = do
  putStr "Specify number of hidden Layers: "
  input <- getLine
  let maybeInt = readMaybe input :: Maybe Int
  case maybeInt of
          Just n  ->
            if n < 1 then
              putStrLn "Numbet of Layers must be at least 1" >> getLayersNumber
            else do
              putStrLn ("Number of Layers = " ++ show n)
              return n
          Nothing -> putStrLn "Please try again."  >> getLayersNumber


-- | Get size of each hidden Layer
getSizes :: Int -> IO [Int]
getSizes numLayers = loop 1
  where
    loop acc = do
      putStr ("Specify size of Layer " ++ show acc ++ ": ")
      input <- getLine
      let maybeInt = readMaybe input :: Maybe Int
      case maybeInt of
          Just n  ->
            if n < 1 then
              putStrLn "Size of Layer must be at least 1" >> loop acc
            else do
              putStrLn ("Size of Layer " ++ show acc ++ " = " ++ show n )
              if acc == numLayers then return [n]
              else do
                s <- loop (acc + 1)
                return (n : s)
          Nothing -> putStrLn "Please try again."  >> loop acc


-- | Get number of training iterations
getEpochs :: IO Int
getEpochs = do
  putStr "Specify number of Epochs: "
  input <- getLine
  let maybeInt = readMaybe input :: Maybe Int
  case maybeInt of
          Just n  ->
            if n < 0 then
              putStrLn "Numbet of Epochs can't be negative" >> getEpochs
            else do
              putStrLn ("Number of Epochs = " ++ show n)
              return n
          Nothing -> putStrLn "Please try again."  >> getEpochs