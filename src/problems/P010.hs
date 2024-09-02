module P010 (main) where

import Util (primeSieve)

main = do
  print $ sum $ primeSieve 2000000
