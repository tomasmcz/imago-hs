module Repa2WX
  ( addEmpty
  , convertArray
  , repa2img
  ) where

import Graphics.UI.WXCore.WxcTypes (Color(..), Point, point, Size, Size2D(..))
import Data.Array.Unboxed hiding (Array)
import Data.Array.MArray
import Data.Array.Unsafe
import Data.Array.Repa as R
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Storable
import Data.Word 
import Foreign.ForeignPtr
import Foreign.Storable

import Codec.Picture.Repa (Img, RGBA)
import Unsafe.Coerce (unsafeCoerce)

data ImgProxy = ImgProxy (Array F DIM3 Word8)

addEmpty :: Array F DIM3 Word8 -> IO (Array F DIM3 Word8)
addEmpty img = computeP $
  R.traverse
    img
    (\ (Z :. x :. y :. _) -> (Z :. x :. y :. sizeOf (0 :: Int)))
    (\ f (Z :. x :. y :. z) ->
      case z of
       3 -> f (Z :. x :. y :. 0)
       2 -> f (Z :. x :. y :. 1)
       1 -> f (Z :. x :. y :. 2)
       _ -> 0
    )

convertArray :: Array F DIM3 Word8 -> IO (Size, [Color])
convertArray arr = do
  let fptr = castForeignPtr $ toForeignPtr arr
      (Z :. h :. w :. _) = extent arr
  stArr <- unsafeForeignPtrToStorableArray fptr (point 0 0, point (w - 1) (h - 1)) :: IO (StorableArray Point Word)
  wArr  <- freeze stArr :: IO (UArray Point Word)
  return (Size w h, (unsafeCoerce <$> elems wArr))
 
repa2img :: Array F DIM3 Word8 -> Img RGBA
repa2img = unsafeCoerce . ImgProxy
