module P098 (main) where

import Data.List
import Data.List.Split
import Data.Ord
import qualified Data.Set as S
import GHC.Plugins
import Util (groupOn, pairs, readInt, readLines, trimOn)

-- | 昇順リストからmin以上max以下の連続した要素を返す。
--
-- >>> filterAsc 10 19 [1,3..]
-- [11,13,15,17,19]
filterAsc :: (Ord a) => a -> a -> [a] -> [a]
filterAsc min max = takeWhile (<= max) . dropWhile (< min)

-- | リストからアナグラムのセットを探す。
--
-- >>> anagrams ["heart","health","earth","aka","aak"]
-- [["aak","aka"],["earth","heart"]]
anagrams :: (Ord a) => [[a]] -> [[[a]]]
anagrams xs = map (map snd) $ filter (`lengthAtLeast` 2) $ groupOn fst $ sort $ map (\w -> (sort w, w)) xs

-- | リストの要素が全て一意であるかどうか。
isDistinct :: (Ord a) => [a] -> Bool
isDistinct xs = S.size (S.fromList xs) == length xs

main = do
  -- 平方アナグラムペア
  --   CARE: 1296=36^2
  --   RACE: 9216=96^2

  ls <- readLines "src/data/0098_words.txt"
  let words = map (trimOn (== '"')) $ splitOn "," $ head ls
  let anagramPairs = sortOn (Down . length . head) $ anagrams words

  let f w1 w2 = do
        let len = length w1
        let squares = map show $ filterAsc (10 ^ (len - 1)) (10 ^ len - 1) $ map (^ 2) [1 ..]
        let anagramNums = anagrams $ filter isDistinct squares
        [ max (readInt n1) (readInt n2)
          | ns <- anagramNums,
            (n1, n2) <- pairs ns,
            sortOn fst (zip w1 n1) == sortOn fst (zip w2 n2)
          ]

  let res = maximum $ concat [f w1 w2 | [w1, w2] <- anagramPairs]
  -- 26ms
  print res
