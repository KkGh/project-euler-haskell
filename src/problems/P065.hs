module P065 (main) where

import Data.Ratio
import Util (digits)

-- | 連分数展開の1番目～n番目の近似分数を返す。
-- > p_n / q_n = [a0; a1, a2, ..., a_n-1]
--
-- >>> convergents 1 [2,2]
-- [1 % 1,3 % 2,7 % 5]
-- >>> take 5 $ convergents 1 [2,2..]
-- [1 % 1,3 % 2,7 % 5,17 % 12,41 % 29]
convergents :: (Integral a) => a -> [a] -> [Ratio a]
convergents a0 as = zipWith (%) nums denoms
  where
    -- n番目の近似分数 p_n/q_n = [a0;a1,a2,...,a_n-1] は以下の数列から計算できる。
    --   p0=1, p1=a0, p_n=(a_n-1*p_n-1 + p_n-2)
    --   q0=0, q1=1,  q_n=(a_n-1*q_n-1 + q_n-2)
    nums = a0 : go 1 a0 as
    denoms = 1 : go 0 1 as
    go _ _ [] = []
    go x y (a : as') = z : go y z as'
      where
        z = x + a * y

main = do
  let es = take 99 $ concatMap (\k -> [1, 2 * k, 1]) [1 ..]
  -- 2ms
  print $ sum $ digits $ numerator $ last $ convergents 2 es
