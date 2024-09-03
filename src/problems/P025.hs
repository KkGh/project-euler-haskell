module P025 (main) where

import Util (digitCount, fibs1)

main = do
  -- 50ms
  print $ snd $ head $ filter (\(n, _) -> digitCount n >= 1000) $ zip fibs1 [1 ..]
