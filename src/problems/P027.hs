module P027 (main) where

import Util (isPrime, maximumOn)

main = do
  -- n^2 + an + b, |a|<1000, |b|<=1000
  -- n=0から連続する素数を生成する a,bの積は？
  let f a b = [n ^ 2 + a * n + b | n <- [0 ..]]
  let ((a, b), _) =
        maximumOn
          snd
          [ ((a, b), length $ takeWhile isPrime $ f a b)
            | a <- [-999 .. 999],
              b <- [-1000 .. 1000]
          ]
  -- 0.65s
  print $ a * b
