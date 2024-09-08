module P062 (main) where

import Data.List
import Safe (findJust)
import Util (groupOn)

main = do
  -- 5つのpermutationsが存在する立方数は？
  let groups = groupOn fst $ sortOn fst [(sort $ show cube, n) | n <- [1 .. 10000], let cube = n ^ 3]
  let n = snd $ head $ findJust (\g -> length g == 5) groups
  -- 33ms
  print $ n ^ 3
