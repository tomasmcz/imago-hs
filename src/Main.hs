{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import Data.Array.Repa as R
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.Algorithms.Pixel
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
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

edges :: Filter
edges = cutoff <=< computeP . mapStencil2 (BoundConst 0) st 
  where
    st = 
      [stencil2| 1  1 1
                 1 -8 1
                 1  1 1 |] 

edges' :: Filter
edges' = cutoff <=< computeP . mapStencil2 (BoundConst 0) st 
  where
    st = 
      [stencil2| 1 1   1 1 1
                 1 1   1 1 1 
                 1 1 -24 1 1
                 1 1   1 1 1 
                 1 1   1 1 1 |] 

peaks :: Filter
peaks = cutoff <=< computeP . mapStencil2 (BoundConst 0) st 
  where
    st = 
      [stencil2| -1 -1 -1
                 -1  8 -1
                 -1 -1 -1 |] 

peaks' :: Filter
peaks' = cutoff <=< computeP . mapStencil2 (BoundConst 0.5) st 
  where
    st = 
      [stencil2| -1 -1  -1 -1 -1
                 -1 -1  -1 -1 -1  
                 -1 -1  24 -1 -1
                 -1 -1  -1 -1 -1  
                 -1 -1  -1 -1 -1 |] 

       
cutoff :: Filter
cutoff = computeP . R.map cut
  where
    {-# INLINE cut #-}
    cut = max 0 . min 1

highpass :: Double -> Filter
highpass h = computeP . R.map (\ x -> if x > h then x else 0) 

lowpass :: Double -> Filter
lowpass l = computeP . R.map (\ x -> if x < l then x else 1)

fromLuminance :: ImageDouble -> IO (Array F DIM2 Word8)
fromLuminance img = computeP $ traverse img id round255
  where
    {-# INLINE round255 #-}
    round255 f (Z :. x :. y) = round . (* 255) $ f (Z :. x :. y)

main :: IO ()
main = do
  [filename] <- getArgs
  (RGB img) <- runIL $ readImage filename
  blured <- gaussBlur =<< toLuminance img 
  edged <- highpass 0.33 =<< normalize =<< edges' blured
  runIL . writeImage "01.bmp" . Grey =<< fromLuminance edged
  houghed <- hough edged
  runIL . writeImage "02.bmp" . Grey =<< fromLuminance houghed
  filtered <- highpass 0.8 =<< peaks houghed
  runIL . writeImage "03.bmp" . Grey =<< fromLuminance filtered
