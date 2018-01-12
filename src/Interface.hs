module Interface (interface) where

import Graphics.Gloss as G
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float
import Data.Fixed


data ImagePicture = Ip
  { iPPixels :: [Float]
  , label :: Int
  , predition :: Int
  } deriving Show


window :: Display
window = InWindow "Nice Window" (800, 800) (10, 10)


background :: Color
background = makeColorI 0 0 0 255


drawing :: [Float] -> Picture
drawing img = pictures $ images ++ [color white $ Text "xD"]
  where
    images = (\ (x, y) -> translate ((-280) + mod' y 28 * 20) (280 + (y / 28) * (-20)) $ color (makeColor x x x 255) $ rectangleSolid 20 20) <$> zip img [0 .. (28 * 28 - 1)]


pixel :: Float -> Float -> Float -> Picture
pixel size p x = G.translate offsetY offsetX $ G.color pixelColor $ G.rectangleSolid size size
  where
    offsetY = (-14 * size) + mod' x 28 * size
    offsetX = (14 * size) + (x / 28) * (-size)
    pixelColor = G.makeColor p p p 255


createTexts :: [ImagePicture] -> Int -> [Picture]
createTexts iP n = [t1,t2]
  where
    s1 = "Current Image: " ++ show (n+1) ++ " OF " ++ show (length iP) ++ " Images"
    s2 = "Label: " ++ show (label (iP !! n)) ++ " Prediction: " ++ show (predition (iP !! n))
    t1 = translate (-280) 340 $ G.scale 0.25 0.25 $ G.color white $ text s1
    t2 = translate (-280) 300 $ G.scale 0.25 0.25 $ G.color white $ text s2


handleKeys :: Event -> ([ImagePicture], Int) -> ([ImagePicture], Int)
handleKeys (EventKey  (SpecialKey KeyLeft) Down _ _) (iPs, n) = if n == 0 then (iPs, n) else (iPs, n - 1)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (iPs, n) = if n == (length iPs - 1) then (iPs, n) else (iPs, n + 1)
handleKeys _ (iPs, n) = (iPs, n)


render :: ([ImagePicture], Int) -> Picture
render (iPs, n) = G.pictures (pixels ++ texts)
  where
    pixels = uncurry (pixel 20) <$> zip (iPPixels (iPs !! n)) [0..(int2Float (length (iPPixels (iPs !! n)) - 1))]
    texts = createTexts iPs n


dummyUpdate f a = a

interface :: [([Float],(Int,Int))] -> IO ()
interface iData = play window background 0 (iPs, 0) render handleKeys dummyUpdate
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