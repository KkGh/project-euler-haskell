module P024 (main) where

import Util (digitsToInt, holes)

-- 順列を辞書順 (lexicographic order) に生成する。
permutationsLex :: [a] -> [[a]]
permutationsLex [] = [[]]
permutationsLex xs = [x : ys | (x, rs) <- holes xs, ys <- permutationsLex rs]

main = do
  -- 0.3s
  print $ digitsToInt $ permutationsLex [0 .. 9] !! (10 ^ 6 - 1)
