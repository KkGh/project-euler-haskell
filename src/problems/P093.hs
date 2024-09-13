{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module P093 (main) where

import Control.Monad
import Data.List
import Data.Ratio
import GHC.Plugins
import Util (digitsToInt, maximumOn)

main = do
  -- 数、演算子、括弧からなる式は以下の5通り。
  --   ((a.b).c).d
  --   (a.(b.c)).d
  --   a.((b.c).d)
  --   a.(b.(c.d))
  --   (a.b).(c.d)

  let _ // 0 = 0
      x // y = x / y
  let nss = [[a, b, c, d] | a <- [1 .. 9], b <- [a + 1 .. 9], c <- [b + 1 .. 9], d <- [c + 1 .. 9]]
      ops = [(+), (-), (*), (//)] :: [Rational -> Rational -> Rational]
  let eval [a, b, c, d] [o, p, q] =
        [ ((a `o` b) `p` c) `q` d,
          (a `o` (b `p` c)) `q` d,
          a `o` ((b `p` c) `q` d),
          a `o` (b `p` (c `q` d)),
          (a `o` b) `p` (c `q` d)
        ]
  let evalAll ns = concat [eval ns' ops' | ns' <- permutations ns, ops' <- replicateM 3 ops]

  let res = map (\ns -> (map numerator ns, length $ f ns)) nss
        where
          f ns =
            map fst
              . takeWhile (uncurry (==))
              . zip [1 ..]
              . nubSort
              $ [numerator r | r <- evalAll ns, denominator r == 1, numerator r > 0]

  -- 0.24s
  print $ digitsToInt . fst $ maximumOn snd res