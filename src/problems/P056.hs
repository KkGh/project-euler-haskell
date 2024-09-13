module P056 (main) where

import Util (digits)

main = do
  -- a^b (a,b<100) の中で最大の桁合計は？
  -- 0.3s
  print $ maximum [sum $ digits $ a ^ b | a <- [1 .. 99], b <- [1 .. 99]]
