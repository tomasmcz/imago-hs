module Camera
  where

import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.HighGui
import Foreign

import Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.Repr.ForeignPtr

import Data.IORef
import System.IO.Unsafe

{-# NOINLINE myGlobalVar #-}
myGlobalVar :: IORef (Ptr CvCapture)
myGlobalVar = unsafePerformIO $ do
  cam <- cvCreateCameraCapture 0
  newIORef cam 

getImage :: IO (Ptr IplImage)
getImage = do
  cam <- readIORef myGlobalVar
  frame <- cvQueryFrame  cam
  return frame

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
