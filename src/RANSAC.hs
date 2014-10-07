module RANSAC
  (
  ) where

import Control.Monad.Random
import Data.List (delete)
import System.Random

type LineParam = (Double, Double, Double)
type Point = (Int, Int)
type LinePoints = (Point, Point)
type Size = (Int, Int)

points2param :: LinePoints -> LineParam
points2param ((a, b), (c, d)) = (fromIntegral $ d - b, fromIntegral $ a - c, fromIntegral $ c * b - a * d)

param2points :: Size -> LineParam -> LinePoints
param2points (w, h) (a, b, c) = undefined

dst :: LineParam -> Point -> Double
dst (a, b, c) (x, y) = abs (a * (fromIntegral x) + b * (fromIntegral y) + c) / sqrt (a * a + b * b)

randomLine :: [Point] -> Rand StdGen LineParam
randomLine lst = do
  p1 <- uniform lst
  p2 <- uniform $ p1 `delete` lst
  return $ points2param (p1, p2)
