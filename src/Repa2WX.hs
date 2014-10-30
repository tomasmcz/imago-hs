{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses #-}

module Repa2WX
  ( addEmpty
  , convertArray
  , repa2img
  ) where

import Graphics.UI.WXCore.WxcTypes
import Data.Array.Unboxed hiding (Array)
import Data.Array.MArray
import Data.Array.Unsafe
import Data.Array.Repa as R
import Data.Array.Repa.Repr.ForeignPtr
import Foreign.ForeignPtr
import Foreign.Storable
import Graphics.UI.WX ()

import Codec.Picture.Repa (Img, RGBA)
import Unsafe.Coerce (unsafeCoerce)

deriving instance IArray UArray Color
deriving instance Storable Color

data ImgProxy = ImgProxy (Array F DIM3 Word8)

addEmpty :: Array F DIM3 Word8 -> IO (Array F DIM3 Word8)
addEmpty img = computeP $
  traverse
    img
    (\ (Z :. x :. y :. _) -> (Z :. x :. y :. 4))
    (\ f (Z :. x :. y :. z) ->
      case z of
       3 -> f (Z :. x :. y :. 0)
       2 -> f (Z :. x :. y :. 1)
       1 -> f (Z :. x :. y :. 2)
       0 -> 0
    )

convertArray :: Array F DIM3 Word8 -> IO (UArray Point Color)
convertArray arr = do
  let fptr = castForeignPtr $ toForeignPtr arr
      (Z :. h :. w :. _) = extent arr
  stArr <- unsafeForeignPtrToStorableArray fptr (point 0 0, point (w - 1) (h - 1))
  fin <- freeze stArr
--  touchForeignPtr fptr
  return fin
 
repa2img :: Array F DIM3 Word8 -> Img RGBA
repa2img = unsafeCoerce . ImgProxy

