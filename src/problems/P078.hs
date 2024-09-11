{-# LANGUAGE BangPatterns #-}

module P078 (main) where

import Data.Maybe
import qualified Data.Vector as V
import Util (partitions)

main = do
  -- 分割数p(n)を1Mで割り切れる最小のnは？

  let ps = partitions 60000
  -- 5.3s
  print $ fromJust $ V.findIndex (\p -> p `mod` 1000000 == 0) ps
