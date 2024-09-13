module P063 (main) where

import Util (digits)

main = do
  let f n = [x ^ n | x <- [1 .. 10], length (digits (x ^ n)) == n]
  -- 2ms
  print $ length $ concatMap f [1 .. 21]
