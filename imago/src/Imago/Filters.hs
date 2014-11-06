{-# LANGUAGE BangPatterns #-}

module Imago.Filters
  ( normalize
  , cutoff
  , highpass
  , lowpass
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

cutoff :: Filter
cutoff = computeP . R.map (max 0 . min 1)

highpass :: Double -> Filter
highpass h = computeP . R.map (\ x -> if x > h then x else 0) 

lowpass :: Double -> Filter
lowpass l = computeP . R.map (\ x -> if x < l then x else 1)
