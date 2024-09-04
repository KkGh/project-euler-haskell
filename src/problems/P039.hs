module P039 (main) where

import Data.List
import qualified Data.Map as M
import Util (maximumOn)

-- | 原始ピタゴラス数 (primitive Pythagorean triples)
-- a^2+b^2=c^2 を満たし、互いに素な自然数の組 (a,b,c) を全て返す。
-- ただし、a < b
--
-- >>> take 5 pythagoreanTriples
-- [(3,4,5),(5,12,13),(8,15,17),(7,24,25),(20,21,29)]
pythagoreanTriples :: [(Integer, Integer, Integer)]
pythagoreanTriples = do
  -- ユークリッド式
  -- 自然数の組 (a,b,c) が原始ピタゴラス数であるためには、ある自然数 m,n が
  --   m,n は互いに素
  --   m>n
  --   m と n の偶奇が異なる
  -- を満たすとして、
  --   (a,b,c) = (m^2−n^2, 2mn, m^2+n^2) または (2mn, m^2−n^2, m^2+n^2)
  -- であることが必要十分である。
  let xs =
        [ (m, n)
          | m <- [2 ..],
            n <- [1 .. m - 1],
            (odd m && even n || even m && odd n) && gcd m n == 1
        ]
  map f xs
  where
    f (m, n) = if o < e then (o, e, c) else (e, o, c)
      where
        o = m ^ 2 - n ^ 2
        e = 2 * m * n
        c = m ^ 2 + n ^ 2

-- 周長がn以下の原始ピタゴラス数を返す。
pythagoreanTriplesLE :: Integer -> [(Integer, Integer, Integer)]
pythagoreanTriplesLE n =
  -- ユークリッド式で生成されるピタゴラス数は昇順にならないため、2N まで生成してフィルタする必要がある
  filter (\(a, b, c) -> (a + b + c) <= n) $
    takeWhile
      (\(a, b, c) -> a + b + c <= n * 2)
      pythagoreanTriples

-- 周長がn以下の全てのピタゴラス数を返す。
-- 結果には原始ピタゴラス数とその整数倍が含まれる。
pythagoreanTriplesLE' :: Integer -> [(Integer, Integer, Integer)]
pythagoreanTriplesLE' n = concatMap f prims
  where
    prims = pythagoreanTriplesLE n
    f (a, b, c) =
      takeWhile (\(a', b', c') -> a' + b' + c' <= n) $
        map (\k -> (k * a, k * b, k * c)) [1 ..]

-- | リスト中の要素とその出現回数をマップにして返す。
--
-- >>> elemCountMap ["a","b","c","a","a","c"]
-- fromList [("a",3),("b",1),("c",2)]
elemCountMap :: (Ord a) => [a] -> M.Map a Int
elemCountMap = foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty

elemCount :: (Ord a) => [a] -> [(a, Int)]
elemCount = M.toList . elemCountMap

main = do
  -- 周長 p<=1000 の直角三角形(a,b,c)の中で、最も直角三角形の種類が多いpは？
  let ts = pythagoreanTriplesLE' 1000
  let counts = elemCount $ map (\(a, b, c) -> a + b + c) ts
  -- <1ms
  print $ fst $ maximumOn snd counts
