module P053 (main) where

import Util (combination)

main = do
  -- 1<=n<=100 のとき、 nCr が10^6 を超えるのは何通りあるか？
  -- 7ms
  print $ length [(n, r) | n <- [1 .. 100], r <- [1 .. n], combination n r >= 10 ^ 6]
