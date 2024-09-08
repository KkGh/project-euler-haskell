module P064 (main) where

import Data.List
import Util (fi)

-- | 自然数の平方根を連分数展開し、初項と循環節からなるリスト [n0,n1,n2,...,n_m] を返す。
-- リスト末尾の要素 n_m は循環節の最終項であり、n0の2倍となる。
--
-- >>> expandContinuedFractionSqrt 7
-- [2,1,1,1,4]
-- >>> expandContinuedFractionSqrt 4
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

main = do
  -- 31ms
  let nonSquares = [2 .. 10000] \\ map (^ 2) [2 .. 100]
  print $ length $ filter (even . length . continuedFractionSqrt) nonSquares
