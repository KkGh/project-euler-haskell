module P072 (main) where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

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

main = do
  -- d<=8 では21個の既約分数が存在する。
  -- d<=1M では？
  let res = VU.sum $ totientSieve (10 ^ 6)
  -- 55ms
  print $ res - 1