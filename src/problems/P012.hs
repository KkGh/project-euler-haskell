module P012 (main) where

import Util

main = do
  -- 約数の個数が500を超える三角数は？
  -- 0.1s
  print $ head $ filter (\n -> divisorFunc 0 n > 500) triangleNumbers

-- 三角数の無限リストを生成する。
triangleNumbers :: [Int]
triangleNumbers = scanl (+) 1 [2 ..]
