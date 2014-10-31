module Main where

import Data.Array.Repa.IO.DevIL
import System.Environment

import Imago.Conv
import Imago.Filters
import Imago.Hough

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
