module P075 (main) where

import Data.List
import Util (pythagoreanTriples)

-- | リストに1度だけ出現する要素を返す。結果のリストはソートされる。
--
-- >>> singles [1,4,3,2,3]
-- [1,2,4]
singles :: (Ord a) => [a] -> [a]
singles xs = concat $ filter (\g -> length g == 1) $ group $ sort xs

main = do
  let limit = 1500000

  -- 直角三角形の辺の長さ = 原始ピタゴラス数の合計
  let lens = takeWhile (<= limit * 2) $ map (\(a, b, c) -> sum [a, b, c]) pythagoreanTriples

  -- ピタゴラス数 = 原始ピタゴラス数を整数倍したもの
  let pithas = concatMap (\l -> takeWhile (<= limit) [l, 2 * l ..]) lens

  let res = singles pithas
  -- 1.3s
  print $ length res
