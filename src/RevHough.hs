module RevHough
  ( hough2LineAD
  , extractHough
  ) where

import Data.Array.Repa as R ((:.)(..), DIM2, DIM3, index, Z(..))
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.Unboxed
import Codec.Picture
import Codec.Picture.Canvas
import Codec.Picture.Repa

data LineAD = LineAD Double Double
type ImageDouble = Array U DIM2 Double
type Point = (Int, Int)
type Line = (Point, Point)

--repa2canvas :: Array F DIM3 Word8 -> Canvas PixelRGBA8
--repa2canvas img =  _ $ imgToImage (Img img :: Img RGBA)

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

lineAD2line :: (Int, Int) -> LineAD -> Line
lineAD2line (w, h) (LineAD angle dist) = 
  if pi < 4 * angle && 4 * angle < 3 * pi
    then
      let y1 = fromIntegral $ (- h) `div` 2
          x1 = (+ w `div` 2) . round $ y1 * cos angle + dist / sin angle 
          y2 = fromIntegral $ h `div` 2
          x2 = (+ w `div` 2) . round $ y2 * cos angle + dist / sin angle 
      in ((x1, 0), (x2, h))
    else
      let x1 = fromIntegral $ (- w) `div` 2
          y1 = (+ h `div` 2) . round $ x1 * sin angle - dist / cos angle 
          x2 = fromIntegral $ w `div` 2
          y2 = (+ h `div` 2) . round $ x2 * sin angle - dist / cos angle 
      in ((0, y1), (w, y2))

--paintLines :: Img -> ImageDouble -> Img
paintLines orig hough = undefined
  where
    size = undefined
    lines = map (lineAD2line size) $ extractHough hough
