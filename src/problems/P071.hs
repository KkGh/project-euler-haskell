module P071 (main) where

import Data.List
import Data.Ratio
import Util (fi)

main = do
  -- 3/7未満で最大の既約分数は？
  -- 各 d で、n/dが最も3/7に近くなる最大の n から順にチェック
  let f d = head [n % d | n <- [nMax, nMax - 1 .. 1], gcd n d == 1]
        where
          nMax = floor $ fi ((3 * d) `div` 7)
  -- 0.25s
  print $ numerator $ foldl' (\maxVal d -> max (f d) maxVal) (2 % 5) [8 .. 10 ^ 6]
