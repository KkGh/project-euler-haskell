module P081 (main) where

import Control.Monad
import qualified Data.Heap as H
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V
import GHC.Plugins (split)
import Util (readLines, (!?))

-- 隣接リスト
type Node = Int

newtype Adj = Adj (M.Map Node [(Node, Float)]) deriving (Show)

-- 始点から終点までの最短距離
dijkstra :: Adj -> Node -> Node -> Float
dijkstra (Adj nodes) s d = go (H.singleton (0, s)) S.empty
  where
    -- ダイクストラ法
    -- 注意：Heap.Heap はタプルの第一要素を priority に使用する。隣接リストと順序が逆なので注意
    go :: H.Heap (Float, Node) -> S.Set Node -> Float
    go pq cs = case H.viewMin pq of
      Nothing -> error "empty PQ"
      Just ((cost, i), pq')
        | i == d -> cost -- 終点ノードの場合は探索終了
        | i `elem` cs -> go pq' cs -- 探索済みノードの場合は無視
        | otherwise -> case M.lookup i nodes of -- それ以外の場合は隣接ノードを展開する
            Nothing -> go pq' cs
            Just neighbors -> do
              let ns = H.fromList [(cost + cost', j) | (j, cost') <- neighbors]
              go (H.union pq' ns) (S.insert i cs)

findMinSum matrix = (matrix V.! 0 V.! 0) + dijkstra adjList 0 (rows * cols - 1)
  where
    rows = V.length matrix
    cols = V.length . V.head $ matrix
    adjList = Adj $ M.fromList $ map (\(i, _) -> (i, neighbors i)) nodes
      where
        -- ノードに 0~N-1 の連番を付ける
        nodes = V.toList $ V.imap (,) $ join matrix
        neighbors i = do
          let right = if i `mod` cols /= (cols - 1) then Just $ nodes !! (i + 1) else Nothing
              down = nodes !? (i + cols)
          catMaybes [right, down]

main = do
  ls <- readLines "src/data/0081_matrix.txt"
  let matrix = V.map (V.fromList . map read . split ',') $ V.fromList ls
  -- 0.5s
  print $ floor $ findMinSum matrix