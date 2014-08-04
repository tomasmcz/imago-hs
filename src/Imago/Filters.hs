{-# LANGUAGE BangPatterns #-}

module Imago.Filters
  (normalize
  ) where

import Data.Array.Repa as R

import Imago.Types

normalize :: Filter
normalize img = do
  let inf = read "Infinity" :: Double
  !maxA <- foldAllP max 0 img
  !minA <- foldAllP min inf img
  let range = maxA - minA
      norm x = (x - minA) / range
  computeP . R.map norm $ img


