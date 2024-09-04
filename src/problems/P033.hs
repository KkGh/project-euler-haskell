module P033 (main) where

import Data.List
import Data.Ratio
import Util (readInt)

main = do
  let res =
        [ (n, d)
          | d <- [10 .. 98],
            n <- [10 .. d - 1],
            n `mod` 10 /= 0,
            d `mod` 10 /= 0,
            d `mod` 10 /= d `div` 10,
            n `mod` 10 /= n `div` 10,
            let common = show n `intersect` show d,
            length common == 1,
            let x = n % d,
            let yn = readInt (show n \\ common),
            let yd = readInt (show d \\ common),
            yd /= 0,
            let y = yn % yd,
            x == y
        ]
  -- 4ms
  print $ denominator $ product $ map (uncurry (%)) res
