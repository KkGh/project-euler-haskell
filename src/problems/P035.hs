module P035 (main) where

import qualified Data.Vector.Unboxed as VU
import Util (digitsToInt, intToDigits, primeSieveV)

-- >>> rotations "abc"
-- ["abc", "bca", "cab"]
rotations xs = take (length xs) $ iterate rotationL xs
  where
    rotationL [] = []
    rotationL (x : xs) = xs ++ [x]

main = do
  -- rotationが全て素数となる数 n < 1M はいくつあるか？

  -- 100未満では13個存在する。
  -- 100以上の（5の倍数以外の）奇数のみチェックすれば良い
  let ps = primeSieveV (10 ^ 6)
  let rotationsInt n = map digitsToInt $ rotations $ intToDigits n
  let isCircularPrime n = all (ps VU.!) $ rotationsInt n

  let candidates = filter (\n -> n `mod` 5 /= 0) [101, 103 .. 10 ^ 6]
  let res = length $ filter isCircularPrime candidates

  -- 0.12s
  print $ 13 + res
