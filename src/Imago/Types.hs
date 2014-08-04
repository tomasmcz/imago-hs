module Imago.Types where

import Data.Array.Repa 

type ImageDouble = Array U DIM2 Double
type Filter = ImageDouble -> IO ImageDouble
