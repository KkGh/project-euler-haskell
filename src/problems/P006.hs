module P006 (main) where

main = do
-- 1~100の2乗の和 - 1~100の和の2乗
  print $ sum [1 .. 100] ^ 2 - sum (map (^ 2) [1 .. 100])
