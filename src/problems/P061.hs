module P061 (main) where

import Data.List

-- xの下2桁とyの上2桁が同じかどうか
isConnectable :: Int -> Int -> Bool
isConnectable x y = x `mod` 100 == y `div` 100

-- 図形数(Figurate Numbers)
triangular :: Int -> Int
triangular n = n * (n + 1) `div` 2

square :: Int -> Int
square n = n ^ 2

pentagonal :: (Integral a) => a -> a
pentagonal n = n * (3 * n - 1) `div` 2

hexagonal :: Int -> Int
hexagonal n = n * (2 * n - 1)

heptagonal :: Int -> Int
heptagonal n = n * (5 * n - 3) `div` 2

octagonal :: Int -> Int
octagonal n = n * (3 * n - 2)

main = do
  -- 6種類の4桁の図形数を配列として得る
  let sets = map figures [triangular, square, pentagonal, hexagonal, heptagonal, octagonal]
      figures f = takeWhile (< 10000) $ dropWhile (< 1000) $ map f [1 ..]
  let cycles = filter (\c -> isConnectable (last c) (head c)) $ go 0 sets
        where
          go n [] = [[n]]
          go 0 sets = concat [go n (sets \\ [set]) | set <- sets, n <- set]
          go n sets = map (n :) succs
            where
              succs = concat [go m (sets \\ [set]) | set <- sets, m <- filter (isConnectable n) set]
  -- 6ms
  print $ sum $ head cycles
