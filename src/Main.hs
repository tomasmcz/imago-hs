module Main where

import Data.Array.Repa as R
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.Algorithms.Pixel
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word
import System.Environment

import Imago.Types
import Imago.Conv
import Imago.Filters
import Imago.Hough

toLuminance :: Array F DIM3 Word8 -> IO ImageDouble
toLuminance img = computeP $ 
  traverse
    img
    (\ (Z :. x :. y :. _) -> (Z :. x :. y))
    luminosity

luminosity :: (DIM3 -> Word8) -> DIM2 -> Double
luminosity f (Z :. i :. j) = doubleLuminanceOfRGB8 (r, g, b)
  where
    r = f (Z :. i :. j :. 0)
    g = f (Z :. i :. j :. 1)
    b = f (Z :. i :. j :. 2)

       
fromLuminance :: ImageDouble -> IO (Array F DIM2 Word8)
fromLuminance img = computeP $ traverse img id round255
  where
    round255 f (Z :. x :. y) = round . (* 255) $ f (Z :. x :. y)

main :: IO ()
main = do
  [filename] <- getArgs
  (RGB img) <- runIL $ readImage filename
  blured <- gaussBlur =<< toLuminance img 
  edged <- highpass 0.33 =<< normalize =<< edges5 blured
  runIL . writeImage "01.bmp" . Grey =<< fromLuminance edged
  houghed <- hough edged
  runIL . writeImage "02.bmp" . Grey =<< fromLuminance houghed
  filtered <- highpass 0.8 =<< peaks3 houghed
  runIL . writeImage "03.bmp" . Grey =<< fromLuminance filtered
