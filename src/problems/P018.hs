module P018 (main) where

import Data.List
import qualified Data.Vector as V
import GHC.Base

-- | 2つのリストの要素を交互に挟み込み、1つのリストを作成する。
--
-- >>> interleave [1,2,3,4] [10,20]
-- [1,10,2,20,3,4]
interleave :: [a] -> [a] -> [a]
interleave xs ys = concat $ transpose [xs, ys]

lastN n xs = drop (length xs - n) xs

triangleN :: (Integral a) => a -> a
triangleN n = n * (n + 1) `div` 2

-- >>> iterateN 4 (\x -> x <> x) "Hi"
-- ["Hi","HiHi","HiHiHiHi","HiHiHiHiHiHiHiHi"]
iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = take n $ iterate f x

-- | 異なる値が連続する先頭部分リストを返す。
--
-- >>> takeWhileChange [3,2,1,1,1]
-- [3,2,1]
takeWhileChange :: (Eq a) => [a] -> [a]
takeWhileChange [] = []
takeWhileChange [x] = [x]
takeWhileChange (x : y : rest)
  | x == y = [x]
  | otherwise = x : takeWhileChange (y : rest)

-- 始点、終点、重み
newtype Edge a = Edge (a, a, Int) deriving (Show, Eq)

-- ベルマンフォード法による単一始点最短距離検索。
-- O(|V|*|E|)
bellmanFord :: (Eq a) => [a] -> [Edge Int] -> a -> V.Vector Int
bellmanFord vs es s = do
  -- 全辺を"緩和"するのを最大 |V|-1 回繰り返す。
  -- 初期化フェーズでは始点以外の頂点の distance を無限大にする。
  -- 緩和フェーズでは各頂点の distance を最短距離に更新していく。
  -- （負の重みの閉路検出は省略）

  -- 各頂点の distance を初期化
  let ds = V.fromList $ map (\v -> if v == s then 0 else maxInt) vs
  let es' = V.fromList es

  -- 辺の緩和を繰り返す（distance の変化がなくなったら終了）
  let result = last $ takeWhileChange $ V.toList $ V.iterateN (length vs) relaxing ds
        where
          relaxing ds = V.foldl' step ds es'
          step ds (Edge (u, v, w)) = do
            let ud = ds V.! u
            let vd = ds V.! v
            if ud + w < vd
              then ds V.// [(v, ud + w)]
              else ds
  result

main = do
  -- 有向非循環グラフの最長経路問題
  -- ベルマンフォード法でエッジの重みを負にすると最長経路検索が可能。

  let triangle =
        V.fromList
          [ V.fromList [75],
            V.fromList [95, 64],
            V.fromList [17, 47, 82],
            V.fromList [18, 35, 87, 10],
            V.fromList [20, 04, 82, 47, 65],
            V.fromList [19, 01, 23, 75, 03, 34],
            V.fromList [88, 02, 77, 73, 07, 63, 67],
            V.fromList [99, 65, 04, 28, 06, 16, 70, 92],
            V.fromList [41, 41, 26, 56, 83, 40, 80, 70, 33],
            V.fromList [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
            V.fromList [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
            V.fromList [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
            V.fromList [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
            V.fromList [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
            V.fromList [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]
          ]

  -- ノードリストとエッジリストを構築する。
  -- ノードのインデックスはルートノードを0として右方向へ増やしていく。
  let indexOf i j = triangleN i + j
  let vs = [indexOf i j | i <- [0 .. V.length triangle - 1], j <- [0 .. i]]
  let es =
        [ Edge (indexOf i j, indexOf i' j', -triangle V.! i' V.! j')
          | i <- [0 .. V.length triangle - 2],
            j <- [0 .. i],
            (i', j') <- [(i + 1, j), (i + 1, j + 1)] -- left, right
        ]

  let dists = V.toList $ bellmanFord vs es 0
  -- print dists
  print $ abs $ -75 + minimum (lastN (length $ V.last triangle) dists)
