module P048 (main) where

import Util (powMod)

main = do
  -- 2ms
  print $ sum (map (\n -> powMod n n (10 ^ 10)) [1 .. 1000]) `mod` 10 ^ 10
