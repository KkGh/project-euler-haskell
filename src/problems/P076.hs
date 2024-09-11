module P076 (main) where

import Control.Monad.ST
import Data.Foldable (for_)
import Data.STRef (modifySTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GHC.STRef

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

main = do
  let ps = partitions 100
  -- <1ms
  print $ (ps V.! 100) - 1