module P002 (main) where

-- フィボナッチ数列の中で400万を超えない偶数の合計
main = do
  print $ sum $ filter even $ takeWhile (< 4000000) fibs

fibs = 1 : go 0 1
  where
    go x y = z : go y z
      where
        z = x + y
