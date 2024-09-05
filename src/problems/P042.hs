module P042 (main) where

import Data.Char
import GHC.Utils.Misc
import Util (readLines, trim)

triangle :: (Integral a) => a -> a
triangle n = n * (n + 1) `div` 2

toWords l = map (trim '\"') $ split ',' l

main = do
  ls <- readLines "src/data/0042_words.txt"
  let words = toWords $ head ls
  let ts = map triangle [1 .. 100]
  -- 5ms
  print $ length [(w, ns) | w <- words, let ns = map (\c -> ord c - 64) w, sum ns `elem` ts]
