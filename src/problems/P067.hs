module P067 (main) where

import Data.List.Split
import qualified Data.Vector as V
import GHC.Base
import Util (takeWhileChange)

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

triangleN :: (Integral a) => a -> a
triangleN n = n * (n + 1) `div` 2

-- リストの末尾N個の要素を返す。
lastN n xs = drop (length xs - n) xs

main = do
  contents <- readFile "src/data/0067_triangle.txt"
  let xss = map (map (\s -> read s :: Int) . splitOn " ") $ lines contents
  let triangle = V.fromList $ map V.fromList xss
  let nodeCount = sum $ V.map V.length triangle
  let root = triangle V.! 0 V.! 0

  -- triangle データからノードリストとエッジリストを構築する。
  -- ノードのインデックスはルートノードを0として右方向へ増やしていく。
  let indexOf i j = triangleN i + j
  let vs = [0 .. nodeCount - 1]
  let es =
        [ Edge (indexOf i j, indexOf i' j', -triangle V.! i' V.! j')
          | i <- [0 .. V.length triangle - 2],
            j <- [0 .. i],
            (i', j') <- [(i + 1, j), (i + 1, j + 1)] -- left, right
        ]

  let dists = V.toList $ bellmanFord vs es 0
  -- 45ms
  print $ abs $ minimum (lastN (length $ V.last triangle) dists) - root
