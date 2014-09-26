{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses #-}

module Repa2WX where

import Graphics.UI.WXCore.WxcTypes
import Data.Array.Unboxed hiding (Array)
import Data.Array.MArray
import Data.Array.Unsafe
import Data.Array.Repa as R
import Data.Array.Repa.Repr.ForeignPtr
import Foreign.ForeignPtr
import Foreign.Storable
import Graphics.UI.WX ()
import Data.Bits

deriving instance IArray UArray Color
deriving instance Storable Color

addEmpty :: Array F DIM3 Word8 -> IO (Array F DIM2 Word)
addEmpty img = computeP $
  traverse
    img
    (\ (Z :. x :. y :. _) -> (Z :. x :. y ))
    (\ f (Z :. x :. y) ->
      let r = fromIntegral $ f (Z :. x :. y :. 0)
          g = fromIntegral $ f (Z :. x :. y :. 1)
          b = fromIntegral $ f (Z :. x :. y :. 2)
      in shift r 24 .|. shift g 16 .|. shift b 8
    )

convertArray :: Array F DIM2 Word -> IO (UArray Point Color)
convertArray arr = do
  let fptr = castForeignPtr $ toForeignPtr arr
      (Z :. h :. w) = extent arr
  stArr <- unsafeForeignPtrToStorableArray fptr (point 0 0, point (w - 1) (h - 1))
  fin <- freeze stArr
--  touchForeignPtr fptr
  return fin
 
