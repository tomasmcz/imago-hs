{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

module Main where

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

import Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Pixel
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word
import Codec.Picture.Repa
--import System.Environment

import Imago.Conv
import Imago.Filters
import Imago.Hough

import Repa2WX
import RevHough

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

makeGrey :: Array F DIM3 Word8 -> IO (Array F DIM3 Word8)
makeGrey img = fromLuminance =<< toLuminance img
 
makeBlur :: Array F DIM3 Word8 -> IO (Array F DIM3 Word8)
makeBlur img = fromLuminance =<< gaussBlur =<< toLuminance img

makeEdges :: Array F DIM3 Word8 -> IO (Array F DIM3 Word8)
makeEdges img = fromLuminance =<< highpass 0.33 =<< normalize =<< edges5 =<< gaussBlur =<< toLuminance img

makeHough :: Array F DIM3 Word8 -> IO (Array F DIM3 Word8)
makeHough img = fromLuminance =<< hough =<< highpass 0.33 =<< normalize =<< edges5 =<< gaussBlur =<< toLuminance img

makeHoughF :: Array F DIM3 Word8 -> IO (Array F DIM3 Word8)
makeHoughF img = fromLuminance =<< highpass 0.33 =<< hough =<< highpass 0.33 =<< normalize =<< edges5 =<< gaussBlur =<< toLuminance img

makeHough2DF :: Array F DIM3 Word8 -> IO ImageDouble
makeHough2DF img = highpass 0.33 =<< hough =<< highpass 0.33 =<< normalize =<< edges5 =<< gaussBlur =<< toLuminance img

dt :: Double
dt = 20 * ms where ms = 1e-3

img2bitmap :: Array F DIM3 Word8 -> IO (Bitmap ())
img2bitmap img = do 
    myimage <- imageCreateFromPixelArray =<< convertArray =<< addEmpty img
    bitmapFromImage myimage

selFunc :: Int -> Img RGBA -> IO RImage
selFunc 7 = \ img -> do 
  hgh <- makeHough2DF . imgData $ img
  return . canvas2repa . paintLines hgh . img2canvas $ img

selFunc 6 = \ img -> do
  hgh <- makeHough2DF . imgData $ img
  
selFunc 5 = makeHoughF . imgData
selFunc 4 = makeHough . imgData
selFunc 3 = makeEdges . imgData
selFunc 2 = makeBlur . imgData
selFunc 1 = makeGrey . imgData
selFunc 0 = pure . id . imgData
selFunc _ = error "selFunc pattern failed"

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
                , "blured"
                , "edges"
                , "Hough"
                , "filtered Hough"
                , "RANSAC"
                , "lines"
                ] []
 
    set f [layout := row 5 [ minsize (sz 520 390) $ widget pp
                           , widget radios
                           ]
          ]

    Right inImg <- readImageRGBA "image.jpg"

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
        
          --etick <- event0 t command
          esel <- event0 radios select
    
          let drawSprite :: Img RGBA -> DC a -> b -> IO ()
              drawSprite img dc _view = do
                sel <- get radios selection
                let tr = selFunc sel
                mybitmap <- img2bitmap =<< tr img
                drawBitmap dc mybitmap (point 0 0) True []
          sink pp [on paint :== pure (drawSprite inImg)]
          reactimate $ repaint pp <$ esel

    network <- compile networkDescription
    actuate network
