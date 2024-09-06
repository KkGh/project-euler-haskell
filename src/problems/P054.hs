module P054 (main) where

import Data.List
import Data.List.Split
import Util (readInt, readLines)

isConsective [] = True
isConsective [_] = True
isConsective (x : y : ys) = (y - x == 1) && isConsective (y : ys)

main = do
  ls <- readLines "src/data/0054_poker.txt"
  -- 1..9JQKA CSHD
  let parseCard ['T', suit] = (10, suit)
      parseCard ['J', suit] = (11, suit)
      parseCard ['Q', suit] = (12, suit)
      parseCard ['K', suit] = (13, suit)
      parseCard ['A', suit] = (14, suit)
      parseCard [n, suit] = (readInt [n], suit)
      parseCard _ = error ""

  let set =
        [ (sort $ map parseCard p1, sort $ map parseCard p2)
          | l <- ls,
            let cards = splitOn " " l,
            let (p1, p2) = splitAt 5 cards
        ]

  -- HighCard: rank 0, Royal Flush: rank 9
  -- (rank, values ascending order)
  let hands :: [(Int, Char)] -> (Int, [Int])
      hands cs
        | isConsective nums = (4, values)
        | any (\(_, c) -> c == 3) groups = (3, values)
        | length (filter (\(_, c) -> c == 2) groups) == 2 = (2, values)
        | length (filter (\(_, c) -> c == 2) groups) == 1 = (1, values)
        | otherwise = (0, values)
        where
          nums = map fst cs
          groups = map (\g -> (head g, length g)) $ group nums
          values = map fst (sortOn snd groups)

  let player1Wins p1 p2 = do
        let (rank1, values1) = hands p1
        let (rank2, values2) = hands p2
        let compareValues (x : xs) (y : ys)
              | x > y = True
              | x < y = False
              | otherwise = compareValues xs ys
            compareValues _ _ = error ""
        (rank1 > rank2) || ((rank1 == rank2) && compareValues (reverse values1) (reverse values2))

  let res = [(player1Wins p1 p2, (p1, h1), (p2, h2)) | (p1, p2) <- set, let h1 = hands p1, let h2 = hands p2]

  -- 15ms
  print $ length $ filter (\(b, _, _) -> b) res
