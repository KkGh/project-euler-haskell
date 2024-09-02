module P020 (main) where

import Util

main = do
  print $ sum $ map (\c -> read [c]) $ show $ factorial 100
