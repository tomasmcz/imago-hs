module Main where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.HighGui

import Control.Monad

import Imago.Conv hiding (fromLuminance, toLuminance)
import Imago.Filters
import Imago.Hough
import Control.Applicative
import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word
import Foreign
import Data.Array.Repa.Algorithms.Pixel


type RepaImg = Array F DIM3 Word8
type ImageDouble = Array U DIM2 Double

convertCV :: Ptr IplImage -> IO (Array F DIM3 Word8)
convertCV p = do
  let sz = cvGetSize p
      x = fromIntegral $ sizeWidth sz
      y = fromIntegral $ sizeHeight sz
  pArr <- peekByteOff p (22 * 4) 
  fp <- newForeignPtr_ pArr
  return $! fromForeignPtr (Z :. y :. x :. 3) $ castForeignPtr fp

bgr2rgb :: Array F DIM3 Word8 -> IO (Array F DIM3 Word8)
bgr2rgb arr = computeP $
  traverse
    arr
    (\ (Z :. y :. x :. z) -> (Z :. y :. x :. z))
    (\ f (Z :. y :. x :. z) -> 
      case z of
        0 -> f (Z :. y :. x :. 2)
        1 -> f (Z :. y :. x :. 1)
        2 -> f (Z :. y :. x :. 0)
        _ -> error "should not happen"
    )

bgr2Luminance :: Array F DIM3 Word8 -> IO ImageDouble
bgr2Luminance img = computeP $ 
  traverse
    img
    (\ (Z :. x :. y :. _) -> (Z :. x :. y))
    luminosity

luminosity :: (DIM3 -> Word8) -> DIM2 -> Double
luminosity f (Z :. i :. j) = doubleLuminanceOfRGB8 (r, g, b)
  where
    r = f (Z :. i :. j :. 2)
    g = f (Z :. i :. j :. 1)
    b = f (Z :. i :. j :. 0)
 
fromLuminance :: ImageDouble -> IO (Array F DIM3 Word8)
fromLuminance img = computeP $
  traverse
    img
    (\ (Z :. x :. y) -> (Z :. x :. y :. 3))
    (\ f (Z :. x :. y :. _) -> round . (* 255) $ f (Z :. x :. y))

liftImago :: (RepaImg -> IO RepaImg) -> Ptr IplImage -> IO ()
liftImago f ptr = do
  arr <- convertCV ptr
  nPtr <- toForeignPtr <$> f arr
  pArr <- peekByteOff ptr (22 * 4) 
  let (Z :. y :. x :. z) = extent arr
  --withForeignPtr nPtr (pokeByteOff ptr (22 * 4))
  withForeignPtr nPtr (\ p -> copyBytes pArr p (x * y * z))

showFrames :: CInt -> Ptr IplImage -> Ptr CvCapture -> IO ()
showFrames winNum targetImage cvcapture  = do
  frame <- cvQueryFrame cvcapture 
  --liftImago pure frame
  --liftImago bgr2rgb frame
  --liftImago (fromLuminance <=< bgr2Luminance) frame
  --liftImago (fromLuminance <=< gaussBlur <=< bgr2Luminance) frame
  --liftImago (fromLuminance <=< normalize <=< edges5 <=< gaussBlur <=< bgr2Luminance) frame
  --liftImago (fromLuminance <=< highpass 0.33 <=< normalize <=< edges5 <=< gaussBlur <=< bgr2Luminance) frame
  liftImago (fromLuminance <=< hough <=< highpass 0.33 <=< normalize <=< edges5 <=< gaussBlur <=< bgr2Luminance) frame
  showImage winNum frame
  key <- waitKey 5
  when (key == -1) (showFrames winNum targetImage cvcapture)
  
processImages :: Ptr CvCapture -> IO ()
processImages capture = do
  frame <- cvQueryFrame capture
  let winNum = 0
  newWindow winNum True
  target <- createImageF (cvGetSize frame) 3 iplDepth8u
  withForeignPtr target (\target' -> showFrames winNum target' capture) 
    
main :: IO ()
main = do
  capture <- createCameraCaptureF 0
  withForeignPtr capture processImages
