module Specs
    ( getLayersNumber, getSizes, getEpochs
    ) where


readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

getLayersNumber :: IO Int
getLayersNumber = do
  putStr "Specify number of hiddent Layers: "
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


getSizes :: Int -> Int -> IO [Int]
getSizes numLayers c = do
  putStr ("Specify size of Layer " ++ show c ++ ": ")
  input <- getLine
  let maybeInt = readMaybe input :: Maybe Int
  case maybeInt of
          Just n  ->
            if n < 1 then
              putStrLn "Size of Layer must be at least 1" >> getSizes numLayers c
            else do
              putStrLn ("Size of Layer " ++ show c ++ " = " ++ show n )
              if c == numLayers then return [n]
              else do
                s <- getSizes numLayers (c + 1)
                return (n : s)
          Nothing -> putStrLn "Please try again."  >> getSizes numLayers c

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