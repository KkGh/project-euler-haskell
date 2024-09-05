module P022 (main) where

import Data.Char
import Data.List
import GHC.Utils.Misc
import Util (readLines, trim)

main = do
  ls <- readLines "src/data/0022_names.txt"
  let words = sort $ map (trim '\"') $ split ',' $ head ls
  let scoreOf name = sum $ [ord c - 64 | c <- name]
  print $ sum [scoreOf name * i | (i, name) <- zip [1 ..] words]
