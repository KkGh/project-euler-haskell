module P034 (main) where

import Data.List
import Util (digits)

-- | 階乗 n! の無限数列 [0!, 1!, 2!, 3!, ...] を生成する。
genFactorial :: (Integral a) => [a]
genFactorial = scanl' (*) 1 [1 ..]

main = do
  let fs = take 10 genFactorial -- 0! ~ 9!
  let res = filter (uncurry (==)) [(n, sum $ map (fs !!) ds) | n <- [3 .. 99999], let ds = digits n]
  -- 30ms
  print $ sum $ map fst res
