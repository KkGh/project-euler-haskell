module P086 (main) where

import Data.List
import Safe (findJust)
import Util (pythagoreanTriples)

solutions m = do
  -- cuboid(w,h,d)上の最短距離が整数になるのは、(w+h,d,最短距離)がピタゴラス数になる場合である。
  -- => ピタゴラス数のaまたはbを分割してcuboidの個数を計算する。

  -- ピタゴラス数とそのk倍を生成する
  let ts = takeWhile (\(a, _, _) -> a <= m * 2) pythagoreanTriples
  let ts' =
        sort
          [ (a * k, b * k)
            | (a, b, _) <- ts,
              k <- [1 .. m `div` a],
              let (a', b') = (a * k, b * k),
              a' <= m && b' <= m * 2,
              b' <= m || b' <= a' * 2
          ]

  -- (a,b)から構成される組(w,h,d)を列挙する。ただし、w+h=a, w<=h<=b, d=bを満たす。
  let f a b = [(w, h, b) | w <- [1 .. a `div` 2], let h = a - w, w <= b && h <= b]

  -- 各ピタゴラス数で、aを分割した場合の個数＋bを分割した場合の個数を計算
  let res = map (\(a, b) -> length (f b a) + if b <= m then length (f a b) else 0) ts'
  sum res

main = do
  -- 24s
  let res = [solutions m | m <- [100 ..]]
  print $ findJust (> 10 ^ 6) res