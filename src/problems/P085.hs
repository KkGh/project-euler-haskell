module P085 (main) where

import Util (minimumOn)

main = do
  -- w*hのグリッドで任意のサイズの長方形を配置する方法の個数
  let countRectInGrid w h = sum $ [(w - m + 1) * (h - n + 1) | n <- [1 .. h], m <- [1 .. w]]
  let counts = [(w, h, countRectInGrid w h) | w <- [1 .. 100], h <- [w .. 100]]
  let (w, h, _) = minimumOn (\(_, _, c) -> abs (c - 2000000)) counts
  -- 0.36s
  print $ w * h
