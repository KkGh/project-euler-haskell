module P069 (main) where

import Util (fi, maximumOn, totient)

main = do
  -- n/φ(n) が大きくなるのは、φ(n)が小さいとき＝nと互いに素な数の個数が少ないとき
  -- 互いに素な数が少なくなる偶数のみチェック
  let ratios = map (\n -> (n, fi n / fi (totient n))) [2, 4 .. 1000000]
  -- 2.4s
  print $ fst $ maximumOn snd ratios