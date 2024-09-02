module P005 (main) where

solve = do
  -- 8.9s
  print $ head . filter (\x -> all (\y -> x `mod` y == 0) [2 .. 20]) $ [20 ..]

solve' = do
  -- 1~20の整数は全て 2^a * 3^b * 5^c * 7^c * 11^c * 13^c * 17^c * 19^c (a<=4, b<=2, c<=1) で表現できる
  -- <1ms
  print $ product [2 ^ 4, 3 ^ 2, 5, 7, 11, 13, 17, 19]

main = do
  solve'
