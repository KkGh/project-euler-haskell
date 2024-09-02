module P015 (main) where

-- 階乗
factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

main = do
  print $ factorial 40 `div` (factorial 20 ^ 2)
