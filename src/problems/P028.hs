module P028 (main) where

main = do
  -- n番目のスパイラルに含まれる4つの数の和をSnとすると
  --   S0=1, S1=sum[3,5,7,9], S2=sum[13,17,21,25], S3=sum[31,37,43,49]
  -- => 等差数列
  let sumSpiral 0 = 1
      sumSpiral n = arithSeries (size * size) (-n * 2) 3
        where
          size = n * 2 + 1
  -- <1ms
  print $ sum $ map sumSpiral [0 .. 500]

-- 初項a0、交差dとして、算術級数（等差数列の第n項までの和）を返す。
arithSeries :: (Integral a) => a -> a -> a -> a
arithSeries a0 d n = (n + 1) * (2 * a0 + n * d) `div` 2
