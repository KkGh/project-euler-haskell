module Util where

import Control.Monad
import Control.Monad.ST
import Data.Bifunctor
import Data.Char (digitToInt)
import Data.Function
import Data.List
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import GHC.Natural

---------------------------------------
-- Math
---------------------------------------

-- 階乗
factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- | nが素数かどうかを判定する。
-- 試し割り法 O(√N)
isPrime :: (Integral a) => a -> Bool
isPrime 2 = True
isPrime n
  | n <= 1 = False
  | even n = False
  | otherwise = go n 3
  where
    -- 3からNの平方根まで順に奇数で割っていく
    go n f
      | f * f > n = True
      | n `mod` f == 0 = False
      | otherwise = go n (f + 2)

-- エラトステネスの篩。
-- Vector版
primeSieveV :: Int -> VU.Vector Bool
primeSieveV n = runST $ do
  -- 0<=i<=n の範囲をTrueで初期化
  v <- VUM.replicate (n + 1) True
  let sqrtN = round $ sqrt $ fi n

  VUM.write v 0 False
  when (n >= 1) $ VUM.write v 1 False

  -- 偶数をFalseにする
  forM_ [4, 6 .. n] $ \i -> do
    VUM.write v i False

  -- 素数の倍数をFalseにする
  forM_ [3, 5 .. sqrtN] $ \i -> do
    isPrime <- VUM.read v i
    when isPrime $ do
      forM_ [i * i, i * i + 2 * i .. n] $ \j -> do
        VUM.write v j False

  VU.freeze v

-- エラトステネスの篩
-- 素数のリストを返す。
primeSieve :: Int -> [Int]
primeSieve n = VU.ifoldr' (\i b acc -> if b then i : acc else acc) [] $ primeSieveV n

-- | 自然数nを素因数分解する。O(√N)
--
-- >>> primeFactors 24
-- [2,2,2,3]
-- >>> primeFactors 19
-- [19]
-- >>> primeFactors 1
-- []
primeFactors :: (Integral a) => a -> [a]
primeFactors n
  | n <= 1 = []
  | otherwise = goTwo n
  where
    -- 試し割り法：2及び3からNの平方根までの奇数で順に割っていく
    goTwo n
      | 4 > n = [n]
      | even n = 2 : goTwo (n `div` 2)
      | otherwise = go n 3
    go n f
      | f * f > n = [n]
      | m == 0 = f : go d f
      | otherwise = go n (f + 2)
      where
        (d, m) = n `divMod` f

-- | 約数関数 σ_x(n): nの約数のx乗和を返す。
--
-- - σ_0(n): nの約数の個数
-- - σ_1(n): nの約数の総和
-- - σ_2(n): nの約数の2乗和
--
-- >>> divisorFunc 1 6
-- 12
divisorFunc :: (Integral a) => a -> a -> a
divisorFunc _ 1 = 1
divisorFunc x n
  | x < 0 = error "x must be x >= 0"
  | x == 0 = product $ map (\(_, a) -> a + 1) fs
  | otherwise = product $ map (\(f, a) -> (f ^ ((a + 1) * x) - 1) `div` (f ^ x - 1)) fs
  where
    fs = map (\g -> (head g, genericLength g)) $ group $ primeFactors n

-- | 自然数nの全ての約数を返す。O(√n)
--
-- >>> divisors 24
-- [1,2,3,4,6,8,12,24]
divisors :: (Integral a) => a -> [a]
divisors n
  | n <= 0 = error $ "Non-natural number"
  | otherwise = map fst fs ++ (if r * r == n then rs' else rs)
  where
    fs = [(x, n `div` x) | x <- [1 .. floor (sqrt (fi n))], n `mod` x == 0]
    rs@(r : rs') = reverse (map snd fs)

-- 累乗の剰余
powMod :: (Integral a) => a -> a -> a -> a
powMod x y m = fi $ powModNatural (fi x) (fi y) (fi m)

---------------------------------------
-- Sequence
---------------------------------------

-- | 0から始まるフィボナッチ数列
--
-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
fibs :: [Integer]
fibs = 0 : 1 : go 0 1
  where
    go x y = z : go y z
      where
        z = x + y

-- | 1から始まるフィボナッチ数列
--
-- >>> take 10 fibs1
-- [1,1,2,3,5,8,13,21,34,55]
fibs1 :: [Integer]
fibs1 = drop 1 fibs

---------------------------------------
-- Combinatorics
---------------------------------------

-- | リストの全てのk-重複組合せを返す。
--
-- >>> combinationsRep 2 "abc"
-- ["aa","ab","ac","bb","bc","cc"]
combinationsRep :: Int -> [a] -> [[a]]
combinationsRep 0 _ = [[]]
combinationsRep k xs = [head ys : zs | ys <- init $ tails xs, zs <- combinationsRep (k - 1) ys]

---------------------------------------
-- Digit
---------------------------------------

-- | 整数値の桁数を返す。
digitCount :: (Integral a, Show a) => a -> Int
digitCount n = length $ show n

-- | 桁リストを整数に変換する。
--
-- >>> digitsToInt [1,2,3,0]
-- 1230
digitsToInt :: (Integral a) => [a] -> a
digitsToInt = foldl1' (\acc x -> acc * 10 + x)

-- | 0以上の整数を桁リストに変換する。
--
-- >>> intToDigits 1230
-- [1,2,3,0]
intToDigits :: Int -> [Int]
intToDigits n
  | n < 0 = error "n must be a non-negative integer"
  | n < 10 = [n]
  | otherwise = intToDigits d ++ [m]
  where
    (d, m) = n `divMod` 10

---------------------------------------
-- List
---------------------------------------

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn f = maximumBy (compare `on` f)

minimumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
minimumOn f = minimumBy (compare `on` f)

groupOn :: (Eq a) => (t -> a) -> [t] -> [[t]]
groupOn f = groupBy (\a b -> f a == f b)

-- | 隣接する全てのN組をリストとして返す。
--
-- >>> adjacents 3 [1,2,3,4,5]
-- [[1,2,3],[2,3,4],[3,4,5]]
adjacents :: Int -> [a] -> [[a]]
adjacents n xs = filter (\l -> length l == n) $ map (take n) $ tails xs

-- | Compute all the ways of removing a single element from a list.
--
-- >>> holes [1,2,3]
-- [(1,[2,3]),(2,[1,3]),(3,[1,2])]
holes :: [a] -> [(a, [a])]
holes [] = []
holes (x : xs) = (x, xs) : (fmap . second) (x :) (holes xs)

---------------------------------------
-- IO
---------------------------------------

readLines :: FilePath -> IO [String]
readLines fileName = lines <$> readFile fileName

readInt :: String -> Int
readInt s = read s :: Int

---------------------------------------
-- Miscs
---------------------------------------

fi n = fromIntegral n

fst3 (x, _, _) = x

snd3 (_, y, _) = y

trd3 (_, _, z) = z

printList xs = mapM_ (\(i, x) -> putStrLn $ show i ++ "\t" ++ show x) $ zip [0 ..] xs

printList1 xs = mapM_ (\(i, x) -> putStrLn $ show i ++ "\t" ++ show x) $ zip [1 ..] xs
