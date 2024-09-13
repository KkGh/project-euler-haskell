module P094 (main) where

import Util (pythagoreanTriples)

-- 周長がn以下のピタゴラス数を返す。
pythagoreanTriplesLE :: Integer -> [(Integer, Integer, Integer)]
pythagoreanTriplesLE n =
  -- ユークリッド式で生成されるピタゴラス数は昇順にならないため、N*2まで生成してフィルタする必要がある
  filter (\(a, b, c) -> (a + b + c) <= n) $
    takeWhile
      (\(a, b, c) -> a + b + c <= n * 2)
      pythagoreanTriples

main = do
  let ts = pythagoreanTriplesLE 1000000000
  let res = filter (\(a, _, c) -> abs (c - a * 2) <= 1) ts
  -- 19s
  print $ sum $ map (\(a, _, c) -> 2 * a + 2 * c) res
