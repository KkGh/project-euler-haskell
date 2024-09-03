module P022 (main) where

import Data.Char
import Data.List
import GHC.Utils.Misc
import Util

-- | リストの先頭と末尾から指定した条件に一致する要素を削除する。
--
-- >>> trimOn (== '#') "#apple#banana##"
-- "apple#banana"
trimOn :: (a -> Bool) -> [a] -> [a]
trimOn p = f . f
  where
    f = dropWhile p . reverse

-- | リストの先頭と末尾から指定した要素を削除する。
trim :: (Eq a) => a -> [a] -> [a]
trim c = trimOn (== c)

main = do
  ls <- readLines "src/data/0022_names.txt"
  let words = sort $ map (trim '\"') $ split ',' $ head ls
  let scoreOf name = sum $ [ord c - 64 | c <- name]
  print $ sum [scoreOf name * i | (i, name) <- zip [1 ..] words]
