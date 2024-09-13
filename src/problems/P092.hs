module P092 (main) where

import Data.Foldable
import qualified Data.Vector.Unboxed.Mutable as VUM
import Util

main = do
  let sumSqDigits n = sum $ map (\x -> x * x) $ digits n

  -- 初期化
  v <- VUM.replicate (10 ^ 7) (0 :: Int)
  VUM.write v 1 1
  VUM.write v 89 89

  -- 再帰的にベクトルの要素に 1 or 89 を割り当てる
  let chain v i = do
        x <- VUM.read v i
        if x /= 0
          then return x
          else chain v (sumSqDigits i) >>= \x' -> VUM.write v i x' >> return x'
  for_ [1 .. 10 ^ 7 - 1] (chain v)

  count <- VUM.foldl' (\acc x -> if x == 89 then acc + 1 else acc) 0 v
  -- 1.6s
  print count
