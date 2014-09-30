module RevHough
  ( hough2LineAD
  , extractHough
  ) where

import Data.Array.Repa as R ((:.)(..), Array(..), DIM2, index, Z(..))
import Data.Array.Repa.Repr.Unboxed

data LineAD = LineAD Double Double
type ImageDouble = Array U DIM2 Double

hough2LineAD :: (Int, Int) -> (Int, Int) -> LineAD
hough2LineAD (iW, iH) (w, h) = LineAD angle dst
  where
    hWidth = 520 :: Int
    hHeight = 390 :: Int
    delta = pi / fromIntegral hHeight :: Double
    initAngle = pi / 4 + delta / 2 
    angle = initAngle + delta * (fromIntegral h)
    (iWidthDiv2, iHeightDiv2) = (iW `div` 2, iH `div` 2)
    maxDst = fromIntegral $ max iHeightDiv2 iWidthDiv2 
    hWidthDiv2 = hWidth `div` 2
    hWidthDiv2DivMax = fromIntegral hWidthDiv2 / maxDst
    dst = fromIntegral w / hWidthDiv2DivMax 

extractHough :: ImageDouble -> [LineAD]
extractHough hImg = map (hough2LineAD (520, 390))
  [ (x, y) 
  | x <- [0..389]
  , y <- [0..519]
  , R.index hImg (Z :. x :. y) > 0
  ]
