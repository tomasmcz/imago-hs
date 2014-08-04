{-# LANGUAGE QuasiQuotes #-}

module Imago.Conv
  (gaussBlur
  ) where

import Control.Monad
import Data.Array.Repa as R
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

import Imago.Types

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
 
