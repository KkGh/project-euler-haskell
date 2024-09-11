module P077 (main) where

import Safe (findJust)
import Util (isPrime)

-- 自然数nを素数で分割する。
partitionPrime n = go n n
  where
    go 0 _ = [[]]
    go 1 _ = []
    go remainder prev = [x : y | x <- filter isPrime [limit, limit - 1 .. 1], y <- go (remainder - x) x]
      where
        limit = min remainder prev

main = do
  -- 60ms
  print $ findJust (\n -> length (partitionPrime n) > 5000) [1 ..]
