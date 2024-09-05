module P045 (main) where

triangular :: (Integral a) => a -> a
triangular n = n * (n + 1) `div` 2

hexagonal :: (Integral a) => a -> a
hexagonal n = n * (2 * n - 1)

pentagonal :: (Integral a) => a -> a
pentagonal n = n * (3 * n - 1) `div` 2

-- | 2つの昇順リストの積集合を返す。
--
-- >>> take 5 $ intersectAsc [3,6..] [5,10..]
-- [15,30,45,60,75]
intersectAsc :: (Ord a) => [a] -> [a] -> [a]
intersectAsc [] _ = []
intersectAsc _ [] = []
intersectAsc xs@(x : xs') ys@(y : ys')
  | x < y = intersectAsc xs' ys
  | x > y = intersectAsc xs ys'
  | otherwise = x : intersectAsc xs' ys'

main = do
  -- 三角数、五角数、六角数となる40755の次の数は？
  let ts = map triangular [1 ..]
  let ps = map pentagonal [1 ..]
  let hs = map hexagonal [1 ..]
  -- 8ms
  print $ head $ dropWhile (<= 40755) $ intersectAsc hs $ intersectAsc ps ts
