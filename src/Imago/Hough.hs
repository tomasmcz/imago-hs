module Imago.Hough
  (hough
  ) where

import Imago.Types
import Imago.Filters

import Control.Monad
import Data.Array.Repa as R
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed as V hiding (replicate)

hough :: Filter
hough img = normalize =<< do
  let hWidth = 520 :: Int
      hHeight = 390
      delta = pi / fromIntegral hHeight :: Double
      initAngle = pi / 4 + delta / 2 
      (Z :. iHeight :. iWidth) = extent img
      (iWidthDiv2, iHeightDiv2) = (iWidth `div` 2, iHeight `div` 2)
      maxDst = fromIntegral $ min iHeight iWidthDiv2
      angles = [(an, initAngle + fromIntegral an * delta) | an <- [0..hHeight - 1]]
      hWidthDiv2 = hWidth `div` 2
      hWidthDiv2DivMax = fromIntegral hWidthDiv2 / maxDst
  h <- V.replicate (hWidth * hHeight) 0
  forM_ [Z :. y :. x | x <- [0..iWidth - 1], y <- [0..iHeight - 1]] $ \ ix -> 
    when (img R.! ix > 0) $
      forM_ angles $ \ (an, angle) -> do
        let (Z :. yi :. xi) = ix
            (x, y) = (fromIntegral $ xi - iWidthDiv2, fromIntegral $ yi - iHeightDiv2) 
            dst = (+ hWidthDiv2) . round . (* hWidthDiv2DivMax) $ x * sin angle + y * cos angle 
        when (dst >= 0 && dst < hWidth) $ do
          let hIndex = an * hWidth + dst
          old <- V.unsafeRead h hIndex 
          V.unsafeWrite h hIndex (old + img R.! ix)
  hv <- V.unsafeFreeze h
  return $ fromUnboxed (Z :. hHeight :. hWidth) hv


