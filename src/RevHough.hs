module RevHough
  ( hough2LineAD
  , extractHough
  , extractHoughPoints
  , img2canvas
  , canvas2repa
  , lineAD2line
  , paintL
  , paintLines
  ) where

import Data.Array.Repa as R ((:.)(..), DIM2, DIM3, index, Z(..))
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.Unboxed
import Data.Word
import Codec.Picture
import Codec.Picture.Canvas
import Codec.Picture.Repa

data LineAD = LineAD Double Double
type ImageDouble = Array U DIM2 Double
type Point = (Int, Int)
type Line = (Point, Point)

img2canvas :: Img RGBA -> Canvas PixelRGBA8
img2canvas img = canvas
  where
    Right canvas = imageToCanvas image
    ImageRGBA8 image = imgToImage $ img

canvas2repa :: Canvas PixelRGBA8 -> Array F DIM3 Word8
canvas2repa = imgData . (convertImage :: Image PixelRGBA8 -> Img RGBA) . canvasToImage 

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
    hWidthDiv2DivMax = maxDst / fromIntegral hWidthDiv2
    dst = fromIntegral (w - hWidthDiv2) * hWidthDiv2DivMax 

extractHough :: (Int, Int) -> ImageDouble -> [LineAD]
extractHough (w, h) = map (hough2LineAD (w, h)) . extractHoughPoints 

extractHoughPoints :: ImageDouble -> [Point]
extractHoughPoints hImg = 
  [ (x, y) 
  | x <- [0..519]
  , y <- [0..389]
  , R.index hImg (Z :. y :. x) > 0
  ]

lineAD2line :: (Int, Int) -> LineAD -> Line
lineAD2line (w, h) (LineAD angle dist) = 
  if pi < 4 * angle && 4 * angle < 3 * pi
    then
      let y1 = fromIntegral $ (- h) `div` 2
          x1 = (+ w `div` 2) . round $ y1 * cos angle + dist / sin angle 
          y2 = fromIntegral $ h `div` 2
          x2 = (+ w `div` 2) . round $ y2 * cos angle + dist / sin angle 
      in ((x1, h), (x2, 0))
    else
      let x1 = fromIntegral $ (- w) `div` 2
          y1 = (+ h `div` 2) . round $ x1 * sin angle - dist / cos angle 
          x2 = fromIntegral $ w `div` 2
          y2 = (+ h `div` 2) . round $ x2 * sin angle - dist / cos angle 
      in ((0, h - y2), (w, h - y1))

paintLines :: ImageDouble -> Canvas PixelRGBA8 -> Canvas PixelRGBA8
paintLines hough orig = foldl (flip draw) orig lns
  where
    draw ((a, b), (c, d)) = drawLine a b c d (PixelRGBA8 0 255 0 0)
    size = (canvasWidth orig, canvasHeight orig)
    lns = map (lineAD2line size) $ extractHough size hough

paintL :: [Line] -> Canvas PixelRGBA8 -> Canvas PixelRGBA8 
paintL lns orig = foldl (flip draw) orig lns
  where
    draw ((a, b), (c, d)) = drawLine a b c d (PixelRGBA8 0 255 0 0)


