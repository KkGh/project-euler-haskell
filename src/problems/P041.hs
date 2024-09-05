module P041 (main) where

import Data.List
import Util (digitCount, intToDigits, primeSieve)
import Safe (findJust)

main = do
  -- n-digit pandigital: 1~n の桁が1度ずつ出現する数
  -- 2143 は 4-digit pandigital
  -- 最大の n-digit pandigital prime は？

  -- 8 digit pandigital は 12345678の順列で3の倍数となる。
  -- 9 digit pandigital は 123456789の順列で3の倍数となる。
  -- => nは最大で 7
  let isPandigitalInt n = (sort . intToDigits) n == [1 .. digitCount n]
  let ps = dropWhile (< 10 ^ 6) $ primeSieve (10 ^ 7)

  -- 0.16s
  print $ findJust isPandigitalInt $ reverse ps
