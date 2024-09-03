module P023 (main) where

import Data.Containers.ListUtils (nubOrd)
import qualified Data.Vector as V
import Util (divisorFunc)

-- 真の約数の和
properDivisorSum n = divisorFunc 1 n - n

isAbandant n = properDivisorSum n > n

main = do
  -- 過剰数: 約数和が自身を超える数。
  -- 28123より大きい数は全て2つの過剰数の和で表現できる。
  -- 2つの過剰数の和で表現できない数の合計は？
  let abundants = V.fromList $ filter isAbandant [1 .. 28123]
  let abundantSums =
        [ a + b
          | i <- [0 .. V.length abundants - 1],
            let a = abundants V.! i,
            j <- [i .. V.length abundants - 1],
            let b = abundants V.! j
        ]
  let total = sum $ nubOrd $ filter (<= 28123) abundantSums

  -- 0.96s
  print $ sum [1 .. 28123] - total -- 28123までの総和 - 「2つの過剰数の和で表現可能な数」の総和
