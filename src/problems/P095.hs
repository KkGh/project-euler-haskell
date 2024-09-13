module P095 (main) where

import Control.Monad.ST
import Data.Foldable
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Util (maximumOn)

-- | 0からnまでの整数kに対する約数関数 σ_x(k) を全て計算する。O(n logn)
--
-- >>> divisorFuncSieveV 1 10
-- [0,1,3,4,7,6,12,8,15,13,18]
divisorFuncSieveV :: Int -> Int -> VU.Vector Int
divisorFuncSieveV x n = runST $ do
  v <- VUM.replicate (n + 1) 1

  VUM.write v 0 0

  for_ [2 .. n] $ \i -> do
    for_ [i, i * 2 .. n] $ \j -> do
      VUM.modify v (+ (i ^ x)) j

  VU.freeze v

-- | リストを重複のない先頭部分リストと残りのリストに分割する。
-- 2つ目のリストが空でない場合、その先頭要素は必ず1つ目のリストに含まれる。
--
-- >>> splitOnDuplicate [3,2,1,2,4]
-- ([3,2,1],[2,4])
-- >>> splitOnDuplicate [3,2,1]
-- ([3,2,1],[])
splitOnDuplicate :: (Ord a) => [a] -> ([a], [a])
splitOnDuplicate [] = ([], [])
splitOnDuplicate xs = go S.empty xs
  where
    go _ [] = ([], [])
    go set (x : xs)
      | x `S.member` set = ([], x : xs)
      | otherwise = (x : ys, zs)
      where
        (ys, zs) = go (S.insert x set) xs

main = do
  -- proper divisors sum: nの真の約数の総和
  let ds = divisorFuncSieveV 1 (10 ^ 6)
  let pds = VU.imap (flip (-)) ds

  -- nがamicable chainの最小値のときのみchainを返す
  let findAmicableChain n =
        case splitOnDuplicate $ takeWhile (\x -> n <= x && x < 10 ^ 6) $ iterate (pds VU.!) n of
          (_, []) -> Nothing
          (xs@(x : _), y : _) -> if x == y then Just xs else Nothing
          _ -> error ""

  let res =
        [ (chain, length chain)
          | n <- [1 .. 10 ^ 6],
            let chainMay = findAmicableChain n,
            isJust chainMay,
            let chain = fromJust chainMay
        ]

  -- 0.13s
  print $ head . fst $ maximumOn snd res