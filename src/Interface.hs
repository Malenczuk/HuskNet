{-|
Module : Interface
|-}
module Interface
  ( interface
  ) where

import Graphics.Gloss as G
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float
import Data.Fixed


-- | data representing labeled image with it's prediction
data ImagePicture = Ip
  { iPPixels :: [Float]
  , label :: Int
  , prediction :: Int
  } deriving Show


-- | Window
window :: Display
window = InWindow "MNIST TEST DATA PREDICTIONS" (800, 800) (10, 10)


-- | Background Color
background :: Color
background = makeColorI 0 0 0 255


-- | Create a rectangle from a image pixel
pixel :: Float -> Float -> Float -> Picture
pixel size p x = G.translate offsetY offsetX $ G.color pixelColor $ G.rectangleSolid size size
  where
    offsetY = (-14 * size) + mod' x 28 * size
    offsetX = (14 * size) + (x / 28) * (-size)
    pixelColor = G.makeColor p p p 255


-- | Create text from labels and predictions
createTexts :: [ImagePicture] -> Int -> [Picture]
createTexts iP n = [t1,t2]
  where
    s1 = "Current Image: " ++ show (n+1) ++ " OF " ++ show (length iP) ++ " Images"
    s2 = "Label: " ++ show (label (iP !! n)) ++ " Prediction: " ++ show (prediction (iP !! n))
    c = if label (iP !! n) == prediction (iP !! n) then G.color white else G.color red
    t1 = translate (-280) 340 $ G.scale 0.25 0.25 $ G.color white $ text s1
    t2 = translate (-280) 300 $ G.scale 0.25 0.25 $ c $ text s2


-- | Simple Key handler
handleKeys :: Event -> ([ImagePicture], Int) -> ([ImagePicture], Int)
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (iPs, n) = if n == 0 then (iPs, n) else (iPs, n - 1)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (iPs, n) = if n == (length iPs - 1) then (iPs, n) else (iPs, n + 1)
handleKeys _ (iPs, n) = (iPs, n)


-- | Render current image and text
render :: ([ImagePicture], Int) -> Picture
render (iPs, n) = G.pictures (pixels ++ texts)
  where
    pixels = uncurry (pixel 20) <$> zip (iPPixels (iPs !! n)) [0..(int2Float (length (iPPixels (iPs !! n)) - 1))]
    texts = createTexts iPs n


-- | Dummy Function
dummyUpdate f (iPs, n) = (iPs, n)


-- | Create an Interface
interface :: [([Float],(Int,Int))] -- ^ Images with their labels and predictions
          -> IO ()
interface iData = G.play window background 0 (iPs, 0) render handleKeys dummyUpdate
  where
    iPs = (\(x,(y,z)) -> Ip x z y ) <$> iData

{-
-- | Play a game in a window.
play :: Display -- ^ Window to draw game in.
     -> Color   -- ^ Background color.
     -> Int     -- ^ Number of simulation steps per second of real time.
     -> a       -- ^ The initial game state.
     -> (a -> Picture)       -- ^ A function to render the world a picture.
     -> (Event -> a -> a)    -- ^ A function to handle input events.
     -> (Float -> a -> a)    -- ^ A function to step the world one iteration.
     -> IO ()
-}