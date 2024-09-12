module P088 (main) where

import Data.Containers.ListUtils (nubOrd)
import Data.List
import Util (groupOn)

-- 各項が2以上、総乗がn以下となる全ての単調増加数列を生成する。
mulPartitionLE :: (Integral a) => a -> [[a]]
mulPartitionLE n = go n 2 []
  where
    -- divide tree
    go n currentMin prefix
      | n < currentMin = []
      | otherwise = concatMap f [currentMin .. n]
      where
        f x = path : go (n `div` x) x path
          where
            path = prefix ++ [x]

main = do
  -- kが12000以下で、総乗が24000以下となる、各項2以上の自然数の組み合わせ（長さ2以上）を列挙する
  let limit = 12000
  let calcK c = product c - sum c + length c
  let xss =
        [ (k, product xs)
          | xs <- mulPartitionLE (limit * 2),
            length xs >= 2,
            let k = calcK xs,
            k <= 12000
        ]

  -- 各kで最小の積を選択
  let groups = groupOn fst $ sort xss
  let minProds = map (minimum . map snd) groups
  -- 0.24s
  print $ sum $ nubOrd minProds
