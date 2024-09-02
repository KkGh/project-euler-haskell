module P016 (main) where

import Data.Char (digitToInt)

-- | 整数の各桁をリストに変換する。
--
-- >>> digits 1230
-- [1,2,3,0]
digits :: (Integral a, Show a) => a -> [Int]
digits n = map digitToInt $ show n

main = do
  print $ sum $ digits $ 2 ^ 1000
