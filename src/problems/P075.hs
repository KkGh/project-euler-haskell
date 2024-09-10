module P075 (main) where

import Data.List

-- | リストに1度だけ出現する要素を返す。結果のリストはソートされる。
--
-- >>> singles [1,4,3,2,3]
-- [1,2,4]
singles :: (Ord a) => [a] -> [a]
singles xs = concat $ filter (\g -> length g == 1) $ group $ sort xs

-- 原始ピタゴラス数 (primitive Pythagorean triples)
-- a^2+b^2=c^2 を満たし、互いに素な自然数の組 (a,b,c) を全て返す。
-- ただし、a < b
primPithagoreans :: [(Integer, Integer, Integer)]
primPithagoreans =
  [ f (m, n)
    | m <- [2 ..],
      n <- [1 .. m - 1],
      (odd m && even n || even m && odd n) && gcd m n == 1
  ]
  where
    f (m, n) = if o < e then (o, e, c) else (e, o, c)
      where
        o = m ^ 2 - n ^ 2
        e = 2 * m * n
        c = m ^ 2 + n ^ 2

main = do
  let limit = 1500000

  -- 直角三角形の辺の長さ = 原始ピタゴラス数の合計
  let lens = takeWhile (<= limit * 2) $ map (\(a, b, c) -> sum [a, b, c]) primPithagoreans

  -- ピタゴラス数 = 原始ピタゴラス数を整数倍したもの
  let pithas = concatMap (\l -> takeWhile (<= limit) [l, 2 * l ..]) lens

  let res = singles pithas
  -- 1.3s
  print $ length res
