module Util (module Util) where

import Control.Monad
import Control.Monad.ST
import Data.Bifunctor
import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Debug.Trace
import GHC.Natural

---------------------------------------
-- Math
---------------------------------------

-- 自然数nが平方数かどうかを判定する。
-- Double精度の問題により、nはIntの範囲に限られる。
isSquareInt :: Int -> Bool
isSquareInt n = (r * r) == n
  where
    r = round $ sqrt $ fromIntegral n

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

-- エラトステネスの篩
-- minFactor[i] はiの最小の素因数。
primeSieveVF :: Int -> (VU.Vector Bool, VU.Vector Int)
primeSieveVF n = runST $ do
  -- 0<=i<=n のベクトルを初期化
  prime <- VUM.replicate (n + 1) True
  minFactor <- VUM.generate (n + 1) id
  let sqrtN = round $ sqrt $ fromIntegral n

  VUM.write prime 0 False
  VUM.write prime 1 False

  -- 偶数を篩い落とす
  for_ [4, 6 .. n] $ \i -> do
    VUM.write prime i False
    VUM.write minFactor i 2

  -- 素数の倍数を篩い落とす
  forM_ [3, 5 .. sqrtN] $ \i -> do
    isPrime <- VUM.read prime i
    when isPrime $ do
      forM_ [i * i, i * i + 2 * i .. n] $ \j -> do
        VUM.write prime j False
        -- 合成数には最小の素因数を設定する
        mf <- VUM.read minFactor j
        when (mf == j) (VUM.write minFactor j i)

  p <- VU.freeze prime
  f <- VU.freeze minFactor
  return (p, f)

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

-- | nの素因数とその指数をタプルリストとして返す。
primeFactorsGroup :: (Integral a, Num b) => a -> [(a, b)]
primeFactorsGroup n = map (\g -> (head g, genericLength g)) $ group $ primeFactors n

-- | minFactorによる素因数分解。
--
-- >>> primeFactorsMF (snd $ primeSieveVF 120) 120
-- [2,2,2,3,5]
primeFactorsMF :: VU.Vector Int -> Int -> [Int]
primeFactorsMF _ 1 = []
primeFactorsMF minFactor n = p : primeFactorsMF minFactor (n `div` p)
  where
    p = minFactor VU.! n

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
  | n <= 0 = error "Non-natural number"
  | otherwise = map fst fs ++ (if r * r == n then rs' else rs)
  where
    fs = [(x, n `div` x) | x <- [1 .. floor (sqrt (fi n))], n `mod` x == 0]
    rs@(r : rs') = reverse (map snd fs)

-- | オイラーのトーシェント関数 φ(n)
-- nと互いに素である1以上n以下の自然数の個数を返す。O(√N)
--
-- >>> totient 6
-- 2
totient :: (Integral a) => a -> a
totient 1 = 1
totient n = round $ fi n * product (map (\p -> 1 - (1 / p)) ps)
  where
    -- 素因数分解から φ(n) を計算する
    ps = map fi $ uniq $ primeFactors n

-- | 0<=i<=n のトーシェント関数 φ(i) を全て計算する。O(n log log n)
--
-- >>> totientSieve 10
-- [0,1,1,2,2,4,2,6,4,6,4]
totientSieve :: Int -> VU.Vector Int
totientSieve n = runST $ do
  -- 正の整数iの素因数を p1, p2, ..., pk とすると、φ(i)は以下で計算できる。
  --   φ(i) = i * (p1-1)/p1 * (p2-1)/p2 * ... * (pk-1)/pk
  -- 篩により φ(i) の値を更新していく。

  -- φ(i)=i で初期化
  phi <- VUM.generate (n + 1) id

  for_ [2 .. n] $ \i -> do
    phi_i <- VUM.read phi i

    when (phi_i == i) $ do
      for_ [i, i * 2 .. n] $ \j -> do
        phi_j <- VUM.read phi j
        VUM.write phi j (phi_j * (i - 1) `div` i)

  VU.freeze phi

-- 累乗の剰余
powMod :: (Integral a) => a -> a -> a -> a
powMod x y m = fi $ powModNatural (fi x) (fi y) (fi m)

-- | 累積和
--
-- >>> cumsum [1,2,3,4,5]
-- [1,3,6,10,15]
cumsum :: (Num a) => [a] -> [a]
cumsum = scanl1 (+)

pentagonal :: (Integral a) => a -> a
pentagonal n = n * (3 * n - 1) `div` 2

-- | 一般五角数(Generalized pentagonal numbers)の無限リストを生成する。
-- m*(3*m - 1)/2, m = 0, +-1, +-2, +-3, ....
--
-- >>> take 10 generalPentagonals
-- [0,1,2,5,7,12,15,22,26,35]
generalPentagonals :: (Integral a) => [a]
generalPentagonals = 0 : concatMap (\n -> map pentagonal [n, -n]) [1 ..]

-- | 0からnまでの整数の分割数を返す。
-- オイラーの五角数定理による実装。
--
-- >>> partitions 7
-- [1,1,2,3,5,7,11,15]
partitions :: (Num a) => Int -> V.Vector a
partitions n = runST $ do
  v <- VM.replicate (n + 1) 0
  VM.write v 0 1

  let signs = cycle [1, 1, -1, -1]

  for_ [1 .. n] $ \k -> do
    let gs = takeWhile (<= k) $ tail generalPentagonals

    -- p(k) = p(k-1) + p(k-2) - p(k-5) - p(k-7) + ...
    pk <- newSTRef 0
    for_ (zip gs signs) $ \(g, sign) -> do
      p <- VM.read v (k - g)
      modifySTRef pk (+ (sign * p))
    readSTRef pk >>= VM.write v k

  V.freeze v

-- | 原始ピタゴラス数 (primitive Pythagorean triples)
-- a^2+b^2=c^2 を満たし、互いに素な自然数の組 (a,b,c) を全て返す。
-- ただし、a < b
--
-- >>> take 5 pythagoreanTriples
-- [(3,4,5),(5,12,13),(8,15,17),(7,24,25),(20,21,29)]
pythagoreanTriples :: [(Integer, Integer, Integer)]
pythagoreanTriples = do
  let xs =
        [ (m, n)
          | m <- [2 ..],
            n <- [1 .. m - 1],
            (odd m && even n || even m && odd n) && gcd m n == 1
        ]
  map f xs
  where
    f (m, n) = if o < e then (o, e, c) else (e, o, c)
      where
        o = m ^ 2 - n ^ 2
        e = 2 * m * n
        c = m ^ 2 + n ^ 2

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

-- | 組み合わせの総数 nCk を計算する。O(k)
--
-- >>> [combination 3 k | k <- [0..3]]
-- [1,3,3,1]
combination :: (Integral a) => a -> a -> a
combination n k
  | k < 0 || n < k = error "combination nCk must be 0<=k<=n"
  | k > n `div` 2 = go n (n - k)
  | otherwise = go n k
  where
    go _ 0 = 1
    -- nCk = n-1Ck-1 * n/k
    go n k = go (n - 1) (k - 1) * n `div` k

-- | リストの全てのペアを返す。
--
-- >>> pairs [1,2,3,4]
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
pairs :: [a] -> [(a, a)]
pairs xs = [(vs V.! i, vs V.! j) | i <- [0 .. len - 1], j <- [i + 1 .. len - 1]]
  where
    vs = V.fromList xs
    len = V.length vs

-- | リストの全てのk-組合せを返す。
--
-- >>> combinations 3 "abcd"
-- ["abc","abd","acd","bcd"]
combinations :: Int -> [a] -> [[a]]
combinations k xs
  | k < 0 = []
  | k == 0 = [[]]
  | otherwise = go k xs
  where
    go 0 _ = [[]]
    go _ [] = []
    go n (y : ys) = map (y :) (go (n - 1) ys) ++ go n ys

-- | リストの全てのk-重複組合せを返す。
--
-- >>> combinationsRep 2 "abc"
-- ["aa","ab","ac","bb","bc","cc"]
combinationsRep :: Int -> [a] -> [[a]]
combinationsRep 0 _ = [[]]
combinationsRep k xs = [head ys : zs | ys <- init $ tails xs, zs <- combinationsRep (k - 1) ys]

-- | 重複順列を列挙する。
--
-- >>> permutationsMultiset [('a',2),('b',2)]
-- ["aabb","abba","abab","bbaa","baab","baba"]
permutationsMultiset :: [(a, Int)] -> [[a]]
permutationsMultiset [] = [[]]
permutationsMultiset sets =
  [ x : xs
    | ((x, c), rest) <- holes sets,
      let sets' = if c > 1 then (x, c - 1) : rest else rest,
      xs <- permutationsMultiset sets'
  ]

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
-- >>> digits 1230
-- [1,2,3,0]
digits :: (Integral a, Show a) => a -> [Int]
digits n
  | n < 0 = error $ "n must be a non-negative integer: " ++ show n
  | otherwise = map digitToInt $ show n

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

-- | リストの先頭と末尾から、指定した条件に一致する要素を削除する。
--
-- >>> trimOn (== '#') "#apple#banana##"
-- "apple#banana"
trimOn :: (a -> Bool) -> [a] -> [a]
trimOn p = f . f
  where
    f = dropWhile p . reverse

trim :: (Eq a) => a -> [a] -> [a]
trim c = trimOn (== c)

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

-- | 異なる値が連続する先頭部分リストを返す。
--
-- >>> takeWhileChange [3,2,1,1,1]
-- [3,2,1]
takeWhileChange :: (Eq a) => [a] -> [a]
takeWhileChange [] = []
takeWhileChange [x] = [x]
takeWhileChange (x : y : rest)
  | x == y = [x]
  | otherwise = x : takeWhileChange (y : rest)

-- | 連続する同じ要素を削除する。O(n)
--
-- >>> uniq "1123321"
-- "12321"
uniq :: (Eq a) => [a] -> [a]
uniq = map head . group

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

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

isqrt :: (Integral a) => a -> a
isqrt n = floor $ sqrt $ fi n

fst3 (x, _, _) = x

snd3 (_, y, _) = y

trd3 (_, _, z) = z

printList xs = mapM_ (\(i, x) -> putStrLn $ show i ++ "\t" ++ show x) $ zip [0 ..] xs

printList1 xs = mapM_ (\(i, x) -> putStrLn $ show i ++ "\t" ++ show x) $ zip [1 ..] xs

traceShow' x = traceShow x x

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise = foldr (\x r k -> case k of 0 -> Just x; _ -> r (k - 1)) (const Nothing) xs n