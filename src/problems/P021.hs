module P021 (main) where

import Util

main = do
  -- d(a) = b, d(b) = a
  let d n = divisorFunc 1 n - n
  print $ sum [a | a <- [2 .. 10000], let b = d a, d b == a, a /= b]
