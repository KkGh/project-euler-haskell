{-# LANGUAGE BangPatterns #-}

module P074 (main) where

import Data.Array
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as VU
import GHC.Utils.Misc (lengthIs)
import Util (digits)

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

takeWhileUniques = fst . splitOnDuplicate

main = do
  -- digit factorial sum テーブル
  let limit = 10 ^ 6
      facts = VU.fromList [1 :: Int, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
      sfd n = sum $ map (facts VU.!) $ digits n
      next n
        | n <= limit = memo ! n
        | otherwise = sfd n
      memo = array (0, limit) [(i, sfd i) | i <- [0 .. limit]]

  let chain n = takeWhileUniques $ iterate next n
  let res = filter (\n -> lengthIs (chain n) 60) [1 .. limit]
  -- 3.7s
  print $ length res