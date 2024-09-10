module P066 (main) where

import Data.List
import Data.Ratio
import Util (fi, maximumOn)

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

-- | 自然数の平方根を連分数展開し、初項と循環節からなるリスト [n0,n1,n2,...,n_m] を返す。
-- リスト末尾の要素 n_m は循環節の最終項であり、n0の2倍となる。
--
-- >>> continuedFractionSqrt 7
-- [2,1,1,1,4]
-- >>> continuedFractionSqrt 4
-- [2]
continuedFractionSqrt :: (Integral a) => a -> [a]
continuedFractionSqrt x = n0 : ns
  where
    -- mが平方数でなければ循環連分数となる。
    -- https://leo.aichi-u.ac.jp/~keisoken/research/books/book51/book51.pdf
    n0 = floor $ sqrt $ fi x
    ns = if n0 * n0 /= x then map (\(n, _, _) -> n) (go n0 1 0) else []
    go n a b
      | n == n0 * 2 = [] -- 循環節の最終項はn0の2倍
      | otherwise = (n', a', b') : go n' a' b'
      where
        b' = n * a - b
        a' = (x - b' ^ 2) `div` a
        n' = (n0 + b') `div` a'

-- ペル方程式 x^2-n*y^2=1 の整数解 (x,y) を返す。
-- nは平方数ではない自然数。
--
-- >>> pell 5
-- (9,4)
pell :: (Integral a) => a -> (a, a)
pell n = if odd (length as) then (p ^ 2 + q ^ 2 * n, 2 * p * q) else (p, q)
  where
    -- 循環節の周期が奇数の場合、(x,y)は右辺＝-1の解となる。
    -- 右辺＝1の解は(x^2+y^2*n, 2*x*y)である。
    p = numerator r
    q = denominator r
    r = last $ convergents a (init as) -- 近似分数
    (a : as) = continuedFractionSqrt n -- √nの連分数

main = do
  -- ペル方程式
  -- y^2 = (x^2-1) / D
  -- Dは平方数でない

  -- 連分数展開と近似分数により、各Dで最小の整数解(x,y)を得る。
  let ds = [2 .. 1000] \\ map (^ 2) [2 .. 100]
  -- 10ms
  print $ maximumOn (fst . pell) ds
