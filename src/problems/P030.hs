module P030 (main) where

import Util (intToDigits)

main = do
  -- 0.56s
  print $ sum $ filter isFifthPower [2 .. 1000000]
  where
    isFifthPower n = n == sum (map (^ 5) . intToDigits $ n)
