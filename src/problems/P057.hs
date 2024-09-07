module P057 (main) where

import Data.Ratio
import Util (digitCount)

-- | 整数列 [a0;a1,..,an] から連分数を生成し、各項を分数形式で返す。
--
-- >>> take 5 $ continuedFractions 1 [2,2..]
-- [3 % 2,7 % 5,17 % 12,41 % 29,99 % 70]
continuedFractions :: Integer -> [Integer] -> [Ratio Integer]
continuedFractions a0 as = zipWith (%) nums denoms
  where
    -- 連分数 pn/qn は以下の数列p, qから計算できる。
    --   p0=1, p1=a0, pn=(a_n-1*p_n-1 + p_n-2)
    --   q0=0, q1=1,  qn=(a_n-1*q_n-1 + q_n-2)
    nums = go 1 a0 as
    denoms = go 0 1 as
    go _ _ [] = []
    go x y (a : as') = z : go y z as'
      where
        z = x + a * y

main = do
  let fs = take 1000 $ continuedFractions 1 [2, 2 ..]
  let res = filter (\r -> digitCount (numerator r) > digitCount (denominator r)) fs
  -- 20ms
  print $ length res
