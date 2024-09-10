module P073 (main) where

import Util (fi)

main = do
  -- 1/3 ~ 1/2 の間に既約分数 n/d (d<=12000) はいくつあるか？
  let res = [(n, d) | d <- [4 .. 12000], n <- [ceiling (fi d / 3) .. floor (fi d / 2)], gcd n d == 1]
  -- 0.75s
  print $ length res
