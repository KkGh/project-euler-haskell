module P096 (main) where

import Data.Containers.ListUtils
import Data.List
import Data.List.Split
import Util (digitsToInt, readInt, readLines, takeWhileChange)

readDigits :: String -> [Int]
readDigits = map (\c -> readInt [c])

isInRange :: (Ord a) => a -> a -> a -> Bool
isInRange lower upper x = lower <= x && x <= upper

(<?) :: (Ord a) => a -> (a, a) -> Bool
(<?) = flip (uncurry isInRange)

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = zipWith f [0 ..]

-- | 指定されたインデックスの要素をリストから削除する。
removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt 0 (_ : xs) = xs
removeAt n (x : xs) = x : removeAt (n - 1) xs

-- | リストのi番目の要素と残りの要素をタプルとして返す。
--
-- >>> select 2 "abcde"
-- ('c',"abde")
select :: Int -> [a] -> (a, [a])
select i xs = (head zs, ys ++ tail zs)
  where
    (ys, zs) = splitAt i xs

-- リストのi番目のみに含まれている要素を返す。
uniqAt :: (Eq a) => Int -> [[a]] -> [a]
uniqAt i xs = foldr (flip (\\)) ys rest
  where
    (ys, rest) = select i xs

box bi bj g = concatMap (take 3 . drop (bj * 3)) $ take 3 $ drop (bi * 3) g

box' i j g = box (i `div` 3) (j `div` 3) g

isFinished = (all . all) (> 0)

makeCands g = [[if e /= 0 then [] else candidatesOf i j g | j <- [0 .. 8], let e = g !! i !! j] | i <- [0 .. 8]]
  where
    candidatesOf i j g = (([1 .. 9] \\ row i g) \\ col j g) \\ box (i `div` 3) (j `div` 3) g
    row i g = g !! i
    col j g = map (!! j) g

-- 確定したセルの周囲にあるセルの候補から数を削除する。
commit i j n (g, c) = (g', c')
  where
    g' = replace i j n g
    c' = replace i j [] $ mapIn (\\ [n]) (i `div` 3 * 3, j `div` 3 * 3) (3, 3) $ mapCol (\\ [n]) j $ mapRow (\c -> c \\ [n]) i c
    replace i j n xss = [[if i == i' && j == j' then n else x | (j', x) <- zip [0 ..] xs] | (i', xs) <- zip [0 ..] xss]
    mapRow f i = imap (\i' xs -> if i' == i then map f xs else xs)
    mapCol f j = map (imap (\j' xs -> if j' == j then f xs else xs))
    mapIn f (i, j) (h, w) = imap g
      where
        g i' xs =
          if i' <? (i, i + h - 1)
            then imap (\j' x -> if j' <? (j, j + w - 1) then f x else x) xs
            else xs

solveBasic grid cand = do
  -- 基本的な問題は以下の2つで解くことが可能：
  --   一つのマスに注目して、そのマスに入る数字を限定する。
  --   一つの列（またはブロック）に注目して、特定の数字が入るマスを探す。
  last $ takeWhileChange $ iterate (\(g, c) -> go (g, c) 0 0) (grid, cand)
  where
    go (g, c) 9 _ = (g, c)
    go (g, c) i 9 = go (g, c) (i + 1) 0
    go (g, c) i j
      | g !! i !! j /= 0 = go (g, c) i (j + 1)
      | otherwise = do
          -- セルの候補リスト - グループの候補リスト で候補が1つだけ残るかチェック
          let uniqsRow = uniqAt j (c !! i)
          let uniqsCol = uniqAt i $ map (!! j) c
          let uniqsBox = uniqAt (3 * (i `mod` 3) + (j `mod` 3)) $ box' i j c
          let (g1, c1) = if length (c !! i !! j) == 1 then commit i j (head $ c !! i !! j) (g, c) else (g, c) -- マスに注目
          let (g2, c2) = if length uniqsRow == 1 then commit i j (head uniqsRow) (g1, c1) else (g1, c1) -- 行に注目
          let (g3, c3) = if length uniqsCol == 1 then commit i j (head uniqsCol) (g2, c2) else (g2, c2) -- 列に注目
          let (g4, c4) = if length uniqsBox == 1 then commit i j (head uniqsBox) (g3, c3) else (g3, c3) -- ブロックに注目
          go (g4, c4) i (j + 1)

refineCands cand = do
  -- ある数字が入る場所が、単一ブロックの同一列に限定されるとき、他のブロックのその列には入らない。
  -- 各ブロックで特定の数字の存在するセルの座標一覧をチェックし、一直線に並んでいる場合に、
  -- 隣接ブロックの同列または同行のセルから候補を除外する。
  go [(bi, bj, n) | bi <- [0, 1, 2], bj <- [0, 1, 2], n <- [1 .. 9]] cand
  where
    go [] cand = cand
    go ((bi, bj, n) : xs) cand = do
      let ps = getNumCoords bi bj n cand
      let cand' = case getDirection ps of
            Just (r, -1) -> removeNeighborRow bj r n cand
            Just (-1, c) -> removeNeighborCol bi c n cand
            _ -> cand
      go xs cand'
    getNumCoords bi bj n cand = [(i, j) | i <- [0 .. 8], j <- [0 .. 8], bi == i `div` 3 && bj == j `div` 3, n `elem` cand !! i !! j]
    removeNeighborCol bi c n cand = transpose $ removeNeighborRow bi c n $ transpose cand
    removeNeighborRow bj r n cand = imap f cand
      where
        f i row = if i /= r then row else imap g row
        g j cs =
          if bj /= j `div` 3 && n `elem` cs
            then delete n cs
            else cs
    getDirection ps
      | length fs == 1 = Just (head fs, -1)
      | length ss == 1 = Just (-1, head ss)
      | otherwise = Nothing
      where
        fs = nubOrd $ map fst ps
        ss = nubOrd $ map snd ps

solveGuessAndTest grid cand = do
  -- 決定的に解けない場合は候補を試し打ち
  find
    (\(g, _) -> isFinished g)
    [ uncurry solveBasic $ commit i j n (grid, cand)
      | (i, row) <- zip [0 ..] cand,
        (j, e) <- zip [0 ..] row,
        n <- e
    ]

solve grid = do
  let (g, c) = solveBasic grid (makeCands grid)
  let (g', c') = solveBasic g (refineCands c)
  if isFinished g'
    then g'
    else case solveGuessAndTest g' c' of
      Just (g'', _) -> g''
      Nothing -> error "unsolved"

main = do
  lines <- readLines "src/data/0096_sudoku.txt"
  let gridStr = map (drop 1) $ chunksOf 10 lines
  let grids = map (\grid -> [readDigits l | l <- grid]) gridStr

  let threeDigitNum g = digitsToInt $ take 3 $ head g

  -- 95ms
  print $ sum $ map (threeDigitNum . solve) grids
