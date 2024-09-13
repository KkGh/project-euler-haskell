module P070 (main) where

import Data.List
import Data.Maybe
import Util (fi, digits, minimumOn, primeSieve, totient)

main = do
  -- φ(n) がnの並べ替えになるようなn (1 < n < 10^7) の中で、比 n/φ(n) が最小となるものは？

  -- n/φ(n) が小さくなるのは、φ(n)が大きい＝nと互いに素な数が多い＝nが奇数のとき
  -- ただし、素数に totient permutation となる数は存在しない
  let isPerm (n, t) = sort (digits n) == sort (digits t)

  -- m,nが互いに素な場合、φ(mn)=φ(m)φ(n)
  let totientCo m n = totient m * totient n

  -- nが2つの素数p,q (p<q)の積の時、比は小さくなる
  -- 小さい方の素数pが大きくなるほど、比は小さくなる
  --   (75841,75184,[149,509],1.0087385613960418)
  --   (84283,83248,[89,947],1.0124327311166634)
  --   (94813,93148,[59,1607],1.017874779920127)
  --   (69271,67912,[53,1307],1.020011190952998)
  --   (45421,44512,[53,857],1.0204214593817398)

  -- 以下を満たす素数の組(a,b)の中で、比が最小になるものを探す
  --   a<b
  --   a*b<10^7
  --   φ(a*b)がa*bの並べ替えになる
  -- aが同じ場合、bが大きい方が比が小さくなる
  let ps = primeSieve $ 10 ^ 4
  let as = takeWhile (< round (sqrt 10 ^ 7)) ps
  let res = mapMaybe f as
        where
          f a = case find (\b -> isPerm (a * b, totientCo a b)) $ reverse bs of
            Just b -> Just (a, b)
            Nothing -> Nothing
            where
              bs = takeWhile (< 10 ^ 7 `div` a) $ dropWhile (< a) ps
  let (a, b) = minimumOn (\(a, b) -> fi (a * b) / fi (totientCo a b)) res
  -- 0.73s
  print $ a * b
