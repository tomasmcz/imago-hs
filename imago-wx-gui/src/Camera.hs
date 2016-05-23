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
myGlobalVar :: IORef Capture
myGlobalVar = unsafePerformIO $ do
  cam <- createCameraCapture 0
  newIORef cam 

getImage :: IO IplImage
getImage = do
  cam <- readIORef myGlobalVar
  queryFrame  cam

convertCV :: IplImage -> IO (Array F DIM3 Word8)
convertCV img = do
  sz <- getSize img
  let x = fromIntegral $ sizeWidth sz
      y = fromIntegral $ sizeHeight sz
  imgData <- getImageData img
  fp <- newForeignPtr_ imgData
  --pArr <- peekByteOff img (22 * 4) 
  --fp <- newForeignPtr_ pArr
  return $! fromForeignPtr (Z :. y :. x :. 3) $ castForeignPtr fp

bgr2rgb :: Array F DIM3 Word8 -> IO (Array F DIM3 Word8)
bgr2rgb arr = computeP $
  R.traverse
    arr
    (\ (Z :. y :. x :. z) -> (Z :. y :. x :. z))
    (\ f (Z :. y :. x :. z) -> 
      case z of
        0 -> f (Z :. y :. x :. 2)
        1 -> f (Z :. y :. x :. 1)
        2 -> f (Z :. y :. x :. 0)
        _ -> error "should not happen"
    )
