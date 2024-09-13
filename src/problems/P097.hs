module P097 (main) where

import Util (powMod)

main = do
  -- メルセンヌ素数： 2^p-1 の形をした素数
  -- 非メルセンヌ素数 28433 * 2^7830457 + 1 の下位10桁は？
  let p = 7830457
  let n = powMod 2 p (10 ^ 10)
  -- <1ms
  print $ (28433 * n + 1) `mod` (10 ^ 10)
