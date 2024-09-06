module P052 (main) where

import Data.List
import Util (intToDigits)

main = do
  -- 77ms
  print $ head $ filter isPermuted [1 ..]
  where
    isPermuted x =
      all
        (\x' -> sort (intToDigits x) == sort (intToDigits x'))
        [x, 2 * x, 3 * x, 4 * x, 5 * x, 6 * x]
