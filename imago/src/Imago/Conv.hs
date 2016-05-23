{-# LANGUAGE QuasiQuotes #-}

module Imago.Conv
  ( gaussBlur
  , edges3
  , edges5
  , peaks3
  , peaks5
  , fromLuminance
  , toLuminance
  ) where

import Control.Monad
import Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Pixel
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Data.Word

import Imago.Types
import Imago.Filters

toLuminance :: Array F DIM3 Word8 -> IO ImageDouble
toLuminance img = computeP $ 
  R.traverse
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
fromLuminance img = computeP $ R.traverse img id round255
  where
    round255 f (Z :. x :. y) = round . (* 255) $ f (Z :. x :. y)

gaussBlur :: Filter
gaussBlur = norm <=< computeP . mapStencil2 (BoundConst 0) gaussStencil
  where
    norm = computeP . R.map (/ 159) :: Filter
    gaussStencil = 
      [stencil2| 2  4  5  4 2
                 4  9 12  9 4
                 5 12 15 12 5
                 4  9 12  9 4
                 2  4  5  4 2 |]
edges3 :: Filter
edges3 = cutoff <=< computeP . mapStencil2 (BoundConst 0) st 
  where
    st = 
      [stencil2| 1  1 1
                 1 -8 1
                 1  1 1 |] 

edges5 :: Filter
edges5 = cutoff <=< computeP . mapStencil2 (BoundConst 0) st 
  where
    st = 
      [stencil2| 1 1   1 1 1
                 1 1   1 1 1 
                 1 1 -24 1 1
                 1 1   1 1 1 
                 1 1   1 1 1 |] 

peaks3 :: Filter
peaks3 = cutoff <=< computeP . mapStencil2 (BoundConst 0.5) st 
  where
    st = 
      [stencil2| -1 -1 -1
                 -1  8 -1
                 -1 -1 -1 |] 

peaks5 :: Filter
peaks5 = cutoff <=< computeP . mapStencil2 (BoundConst 0.5) st 
  where
    st = 
      [stencil2| -1 -1  -1 -1 -1
                 -1 -1  -1 -1 -1  
                 -1 -1  24 -1 -1
                 -1 -1  -1 -1 -1  
                 -1 -1  -1 -1 -1 |] 


