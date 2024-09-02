module P001 (main) where

-- 1000未満の3または5の倍数の合計
main = do
  print $ sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1 .. 999]
