{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses #-}

module Main where

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

import Data.Array.Repa as R
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.Algorithms.Pixel
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word
--import System.Environment

import Graphics.UI.WXCore.WxcTypes
import Data.Array.Unboxed hiding (Array)
import Data.Array.MArray
import Data.Array.Unsafe
import Foreign.ForeignPtr
import Foreign.Storable
import Data.Bits

deriving instance IArray UArray Color
deriving instance Storable Color

{-
main :: IO ()
main = do
  [filename] <- getArgs
  (RGB img) <- runIL $ readImage filename
  blured <- gaussBlur =<< toLuminance img 
  edged <- highpass 0.33 =<< normalize =<< edges5 blured
  runIL . writeImage "01.bmp" . Grey =<< fromLuminance edged
-}

type ImageDouble = Array U DIM2 Double
type Filter = ImageDouble -> IO ImageDouble
type RImage = Array F DIM3 Word8

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
       
fromLuminance :: ImageDouble -> IO (Array F DIM3 Word8)
fromLuminance img = computeP $
  traverse
    img
    (\ (Z :. x :. y) -> (Z :. x :. y :. 3))
    (\ f (Z :. x :. y :. _) -> round . (* 255) $ f (Z :. x :. y))

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

makeGrey :: Array F DIM3 Word8 -> IO (Array F DIM3 Word8)
makeGrey img = fromLuminance =<< toLuminance img

convertArray :: Array F DIM2 Word -> IO (UArray Point Color)
convertArray arr = do
  let fptr = castForeignPtr $ toForeignPtr arr
      (Z :. h :. w) = extent arr
  stArr <- unsafeForeignPtrToStorableArray fptr (point 0 0, point (w - 1) (h - 1))
  fin <- freeze stArr
--  touchForeignPtr fptr
  return fin
  
dt :: Double
dt = 20 * ms where ms = 1e-3

img2bitmap :: Array F DIM3 Word8 -> IO (Bitmap ())
img2bitmap img = do 
    myimage <- imageCreateFromPixelArray =<< convertArray =<< addEmpty img
    bitmapFromImage myimage

selFunc :: Int -> RImage -> IO RImage
selFunc 1 = makeGrey
selFunc _ = pure . id

main :: IO ()
main = start $ do
    f <- frame [ text := "Hokus Pokus"
               ]
    --t  <- timer f [interval := ceiling (dt * 1e3)]
    pp <- panel f [ bgcolor := white
                  ]
    radios <- radioBox f Vertical 
                [ "original"
                , "grey-scale"
                ] []
 
    set f [layout := row 5 [ minsize (sz 400 300) $ widget pp
                           , widget radios
                           ]
          ]

    (RGB inImg) <- runIL $ readImage "image.jpg"

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
        
          --etick <- event0 t command
          esel <- event0 radios select
    
          let drawSprite :: RImage -> DC a -> b -> IO ()
              drawSprite img dc _view = do
                sel <- get radios selection
                let tr = selFunc sel
                mybitmap <- img2bitmap =<< tr img
                drawBitmap dc mybitmap (point 0 0) True []
          sink pp [on paint :== pure (drawSprite inImg)]
          reactimate $ repaint pp <$ esel

    network <- compile networkDescription
    actuate network
