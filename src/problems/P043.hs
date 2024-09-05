module P043 (main) where

import Data.List
import Util (adjacents, digitsToInt)

main = do
  let check ds = all (\(v, p) -> v `mod` p == 0) $ zip substrs ps
        where
          substrs = map digitsToInt $ tail $ adjacents 3 ds
          ps = [2, 3, 5, 7, 11, 13, 17]
  let ns = [digitsToInt ds | ds <- permutations [0 .. 9], check ds]
  -- 0.9s
  print $ sum ns