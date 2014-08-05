{-# LANGUAGE QuasiQuotes #-}

module Imago.Conv
  ( gaussBlur
  , edges3
  , edges5
  , peaks3
  , peaks5
  ) where

import Control.Monad
import Data.Array.Repa as R
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

import Imago.Types
import Imago.Filters

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


