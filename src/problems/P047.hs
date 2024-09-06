module P047 (main) where

import Safe (findJust)
import Util (adjacents, primeFactorsGroup)

main = do
  -- 以下を満たす4連続の整数の1つ目の数は？
  --   整数は3つの異なる素数の積
  let ts = adjacents 4 [1 ..]
  let f x = length (primeFactorsGroup x) == 4
  let result = findJust (all f) ts
  -- 0.5s
  print $ head result
