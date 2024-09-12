module P082 (main) where

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

-- 始点からいずれかの終点までの最短距離
dijkstra' :: Adj -> Node -> [Node] -> Float
dijkstra' (Adj nodes) s ds = go (H.singleton (0, s)) S.empty
  where
    -- ダイクストラ法
    -- 注意：Heap.Heap はタプルの第一要素を priority に使用する。隣接リストと順序が逆なので注意
    go :: H.Heap (Float, Node) -> S.Set Node -> Float
    go pq cs = case H.viewMin pq of
      Nothing -> error "empty PQ"
      Just ((cost, i), pq')
        | i `elem` ds -> cost -- いずれかの終点に到達した場合に探索終了
        | i `elem` cs -> go pq' cs -- 探索済みノードの場合は無視
        | otherwise -> case M.lookup i nodes of -- それ以外の場合は隣接ノードを展開する
            Nothing -> go pq' cs
            Just neighbors -> do
              let ns = H.fromList [(cost + cost', j) | (j, cost') <- neighbors]
              go (H.union pq' ns) (S.insert i cs)

findMinSum matrix = minimum lens
  where
    lens = map (\s -> weightOf s + dijkstra' adjList s di) si
    weightOf i = matrix V.! (i `div` cols) V.! (i `mod` cols)
    si = [0, 80 .. (rows - 1) * cols]
    di = [79, 159 .. rows * cols - 1]
    rows = V.length matrix
    cols = V.length . V.head $ matrix
    adjList = Adj $ M.fromList $ map (\(i, _) -> (i, neighbors i)) nodes
      where
        -- ノードに 0~N-1 の連番を付ける
        nodes = V.toList $ V.imap (,) $ join matrix
        neighbors i = do
          let right = if i `mod` cols /= (cols - 1) then Just $ nodes !! (i + 1) else Nothing
          let down = nodes !? (i + cols)
          let up = nodes !? (i - cols)
          catMaybes [right, down, up]

main = do
  ls <- readLines "src/data/0082_matrix.txt"
  let matrix = V.map (V.fromList . map read . split ',') $ V.fromList ls
  -- 12s
  print $ floor $ findMinSum matrix
