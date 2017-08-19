module Imago.RANSAC
  ( points2param
  , param2points
  , consensus
  , dst
  , rIterate
  , randomLine
  , ransac
  , ransac2
  ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Random
import Data.List (delete, (\\))
import System.Random ()

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

type LineParam = (Double, Double, Double)
type Point = (Int, Int)
type LinePoints = (Point, Point)
--type Size = (Int, Int)

points2param :: LinePoints -> LineParam
points2param ((a, b), (c, d)) = (fromIntegral $ d - b, fromIntegral $ a - c, fromIntegral $ c * b - a * d)

param2points :: LineParam -> LinePoints
param2points (a, b, c) = ((0, round $ (-c) / b), (round $ (-c) / a, 0))

dst :: LineParam -> Point -> Double
dst (a, b, c) (x, y) = abs (a * fromIntegral x + b * fromIntegral y + c) / sqrt (a * a + b * b)

randomLine :: [Point] -> Rand StdGen LineParam
randomLine lst = do
  p1 <- uniform lst
  p2 <- uniform $ p1 `delete` lst
  return $ points2param (p1, p2)

consensus :: LineParam -> [Point] -> [Point]
consensus l = filter (\ p -> dst l p < 3)

rIterate :: [Point] -> LineParam -> (Int, LineParam, [Point])
rIterate ps l = f (c, length c)
  where
    c = consensus l ps
    f = fix $ \ loop (cns, s) -> do 
      let nextLn = leastSquares cns
          nextCns = consensus nextLn ps
          nextS = length nextCns
      if nextS > s
        then loop (nextCns, nextS)
        else (s, nextLn, nextCns)

ransac :: [Point] -> Int -> Rand StdGen (LineParam, [Point])
ransac ps n = do
  lst <- replicateM n (randomLine ps)
  let (_, l, c) = maximum $ map (rIterate ps) lst
  return (l, c)
    
ransac2 :: [Point] -> Int -> Rand StdGen [(LineParam, [Point])]
ransac2 ps n = do
  lst <- replicateM n (randomLine ps)
  let (_, l, c) = maximum $ map (rIterate ps) lst
      np = ps \\ c
  lst2 <- replicateM n (randomLine np)
  let (_, l2, c2) = maximum $ map (rIterate np) lst2
  return [(l, c), (l2, c2)]
    
leastSquares :: [Point] -> LineParam
leastSquares ps = (a, -1, c)
  where
    x  = fromLists [[fromIntegral q, 1] | (q, _) <- ps] 
    y  = fromLists [[fromIntegral r]    | (_, r) <- ps] 
    [[a], [c]] = toLists $ linearSolveLS x y
