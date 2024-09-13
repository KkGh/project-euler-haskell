module P099 (main) where

import GHC.Utils.Misc (split)
import Safe (findIndexJust)
import Util

digitExp x y = floor (y * logBase 10 x) + 1

main = do
  ls <- readLines "src/data/0099_base_exp.txt"
  let xs = map (map readInt . split ',') ls
  let res = maximumOn (\[x, y] -> digitExp (fi x) (fi y)) xs
  -- 7ms
  print (findIndexJust (== res) xs + 1)
