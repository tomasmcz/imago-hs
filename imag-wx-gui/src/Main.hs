{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

module Main where

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

import Data.Array.Repa as R hiding (map)
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word
import Codec.Picture.Repa
--import System.Environment

import Control.Monad.Random

import Imago.Conv hiding (fromLuminance)
import Imago.Filters
import Imago.Hough
import Imago.RANSAC

import Repa2WX
import RevHough
import Camera

type ImageDouble = Array U DIM2 Double
type Filter = ImageDouble -> IO ImageDouble
type RImage = Array F DIM3 Word8

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

img2bitmap :: Array F DIM3 Word8 -> IO (Bitmap ())
img2bitmap img = do 
    myimage <- imageCreateFromPixelArray =<< convertArray =<< addEmpty img
    bitmapFromImage myimage

selFunc :: Int -> Img RGBA -> IO (Bitmap ())

selFunc 8 img = do 
  hgh <- makeHough2DF . imgData $ img
  img2bitmap . canvas2repa . paintLines hgh . img2canvas $ img

selFunc 7 img = do
  hgh <- makeHough2DF . imgData $ img
  let (Z :. h :. w :. _) = extent . imgData $ img
      points = extractHoughPoints hgh
  rlines <- evalRandIO $ ransac2 points 20
  let lns = map (lineAD2line (w, h) . hough2LineAD (w, h)) $ concatMap snd rlines 
  img2bitmap . canvas2repa . paintL lns . img2canvas $ img
 
selFunc 6 img = do
  hgh <- makeHough2DF . imgData $ img
  let points = extractHoughPoints hgh
  rlines <- evalRandIO $ ransac2 points 20
  let lns = map (param2points . fst) rlines
  img2bitmap . canvas2repa . paintL lns . img2canvas . repa2img =<< fromLuminance hgh
  
selFunc 5 img = liftImg makeHoughF img
selFunc 4 img = liftImg makeHough img
selFunc 3 img = liftImg makeEdges img
selFunc 2 img = liftImg makeBlur img
selFunc 1 img = liftImg makeGrey img
selFunc 0 img = liftImg pure img
selFunc _ _ = error "selFunc pattern failed"

liftImg :: (RImage -> IO RImage) -> Img RGBA -> IO (Bitmap ())
liftImg f img = img2bitmap =<< f (imgData img)

selInput :: Int -> IO (Img RGBA)
selInput 0 = do
  Right im <- readImageRGBA "image.jpg"
  return im
selInput 1 = repa2img <$> (bgr2rgb =<< convertCV =<< getImage)
selInput _ = error "selInput pattern failed"

main :: IO ()
main = start $ do
    f <- frame [ text := "Hokus Pokus"
              ]
    t  <- timer f [interval := 1000]
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
                , "RANSAC lines"
                , "lines"
                ] []
    radiosIn <- radioBox f Vertical
                [ "file"
                , "camera"
                ] []
 
    set f [layout := row 5 [ minsize (sz 640 480) $ widget pp
                           , widget radios
                           , widget radiosIn
                           ]
          ]


    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
        
          etick <- event0 t command
          esel <- event0 radios select
          eselIn <- event0 radiosIn select
    
          let drawSprite :: DC a -> b -> IO ()
              drawSprite dc _view = do
                sel <- get radios selection
                let tr = selFunc sel
                selIn <- get radiosIn selection
                img <- selInput selIn
                mybitmap <- tr img
                drawBitmap dc mybitmap (point 0 0) True []
          sink pp [on paint :== pure drawSprite]
          reactimate $ repaint pp <$ esel `union` eselIn `union` etick

    network <- compile networkDescription
    actuate network
