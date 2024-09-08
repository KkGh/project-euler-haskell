module P063 (main) where

import Util (intToDigits)

main = do
  let f n = [x ^ n | x <- [1 .. 10], length (intToDigits (x ^ n)) == n]
  -- 2ms
  print $ length $ concatMap f [1 .. 21]
