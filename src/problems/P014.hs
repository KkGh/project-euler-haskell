module P014 (main) where

import Data.Array
import Data.Function
import Data.List
import Util

collatzSequence :: Int -> [Int]
collatzSequence n
  | n == 1 = [1]
  | even n = n : collatzSequence (n `div` 2)
  | otherwise = n : collatzSequence (3 * n + 1)

collatzStep n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

solve = do
  -- 1.8s
  print $ head $ maximumBy (compare `on` length) $ map collatzSequence [1 .. 1000000]

solve2 = do
  -- DP
  -- l(13) = 1+l(40)
  -- l(40) = 1+l(20)
  -- l(20) = ...
  let limit = 10 ^ 6
  let chainLength :: Int -> Int
      chainLength n = memo ! n
      memo = array (1, limit) [(i, go i) | i <- [1 .. limit]]
      go 1 = 1
      go n = 1 + if n' <= limit then memo ! n' else go n'
        where
          n' = collatzStep n

  -- 0.18s
  print $ maximumOn chainLength [1 .. 10 ^ 6]

main = do
  solve2
