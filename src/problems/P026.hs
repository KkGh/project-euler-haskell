module P026 (main) where

import Util (divisors, powMod, primeSieve)

-- 基数10における素数の逆数 1/p の循環節 (repetend) の長さを返す。
repetendLength 2 = 1
repetendLength 5 = 1
repetendLength p = do
  -- 10^k≡1 (mod p) を満たす最小のkを求める。ただし、kはp-1のいずれかの約数。
  head $ filter (\k -> powMod 10 k p == 1) $ divisors (p - 1)

main = do
  -- 基数は10（10進数）
  -- 分母が2,5以外の因数を持っている場合、循環小数になる。
  -- 循環節の長さの上限は分母-1。
  -- 素数pについて、 1/p の循環節の長さは p-1 の約数のいずれかである。
  -- 循環節長が p-1 になる最大の素数pを探す。

  let ps = primeSieve 1000
  print $ maximum $ filter (\p -> repetendLength p == p - 1) ps
