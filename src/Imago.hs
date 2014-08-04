{-# LANGUAGE FlexibleContexts, BangPatterns, QuasiQuotes #-}

import Control.Monad
import Data.Array.Repa hiding ((++))
import qualified Data.Array.Repa as R
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.Algorithms.Pixel
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed as V hiding (replicate)
import Data.Word

type ImageDouble = Array U DIM2 Double
type Filter = ImageDouble -> IO ImageDouble

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

gaussBlur :: Filter
gaussBlur = computeP . R.map (/ 159) . mapStencil2 (BoundConst 0) gaussStencil 
  where
    gaussStencil = 
      [stencil2| 2  4  5  4 2
                 4  9 12  9 4
                 5 12 15 12 5
                 4  9 12  9 4
                 2  4  5  4 2 |]
        
cutoff :: Filter
cutoff = lowpass 1 <=< highpass 0

highpass :: Double -> Filter
highpass h = computeP . R.map (\ x -> if x > h then x else 0)

lowpass :: Double -> Filter
lowpass l = computeP . R.map (\ x -> if x < l then x else 1)

normalize :: Filter
normalize img = normalize'
  (foldAllS max 0 img)
  (foldAllS min (read "Infinity") img)
  img

normalize' :: Double -> Double -> Filter
normalize' !maxA !minA = computeP . R.map (\ x -> (x - minA) / range) 
  where
    range = maxA - minA

fromLuminance :: ImageDouble -> IO (Array F DIM2 Word8)
fromLuminance img = computeP $ traverse img id round255
  where round255 f (Z :. x :. y) = round . (* 255) $ f (Z :. x :. y)

hough :: Filter
hough img = normalize =<< do
  let hWidth = 800 :: Int
      hHeight = 600
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
          old <- V.read h hIndex 
          V.write h hIndex (old + img R.! ix)
  hv <- V.freeze h
  return $ fromUnboxed (Z :. hHeight :. hWidth) hv

main :: IO ()
main = do
  (RGB img) <- runIL $ readImage "image.jpg"
  blured <- gaussBlur =<< toLuminance img 
  edged <- highpass 0.33 =<< normalize =<< edges' blured
  runIL . writeImage "01.bmp" . Grey =<< fromLuminance edged
  houghed <- hough edged
  runIL . writeImage "02.bmp" . Grey =<< fromLuminance houghed
  filtered <- highpass 0.8 =<< peaks houghed
  runIL . writeImage "03.bmp" . Grey =<< fromLuminance filtered
