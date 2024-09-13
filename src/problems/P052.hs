module P052 (main) where

import Data.List
import Util (digits)

main = do
  -- 77ms
  print $ head $ filter isPermuted [1 ..]
  where
    isPermuted x =
      all
        (\x' -> sort (digits x) == sort (digits x'))
        [x, 2 * x, 3 * x, 4 * x, 5 * x, 6 * x]
