module P038 (main) where

import Data.List
import Util (intToDigits, readInt)

isPandigital n = sort (intToDigits n) == [1 .. 9]

main = do
  -- 整数がパンデジタル数かどうか。
  -- パンデジタル数：1~9の全ての数を1度ずつ使っている数

  let f v =
        takeWhile (\xs -> length xs < 10) $
          dropWhile (\xs -> length xs < 9) $
            scanl1 (++) [show $ v * n | n <- [1 ..]]
  -- 15ms
  print $ readInt $ maximum [c | v <- [1 .. 10000], c <- f v, isPandigital (read c)]
