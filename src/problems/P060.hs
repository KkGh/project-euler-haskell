module P060 (main) where

import qualified Data.Vector.Unboxed as VU
import Util (digitCount, isPrime, primeSieveV)

concatInt :: (Integral a, Show a) => a -> a -> a
concatInt x y = x * 10 ^ digitCount y + y

trueIndices :: VU.Vector Bool -> [Int]
trueIndices = VU.ifoldr' (\i b acc -> if b then i : acc else acc) []

main = do
  -- 素数 a,b,c,d,e
  -- どの2つの素数を連結させても素数となる5つの素数の合計は？

  let psv = primeSieveV (10 ^ 8)
      isPrime' n
        | n < 10 ^ 8 = psv VU.! n
        | otherwise = isPrime n

  -- 4桁までの素数の組み合わせをチェック
  let ps = VU.fromList $ takeWhile (< 10000) $ trueIndices psv
      lastIndex = VU.length ps - 1
  let concatable x ys = all (\y -> isPrime' (concatInt x y) && isPrime' (concatInt y x)) ys

  let res =
        [ ((a, b, c, d, e), a + b + c + d + e)
          | ai <- [0 .. lastIndex],
            let a = ps VU.! ai,
            bi <- [ai + 1 .. lastIndex],
            let b = ps VU.! bi,
            concatable b [a],
            ci <- [bi + 1 .. lastIndex],
            let c = ps VU.! ci,
            concatable c [a, b],
            di <- [ci + 1 .. lastIndex],
            let d = ps VU.! di,
            concatable d [a, b, c],
            ei <- [di + 1 .. lastIndex],
            let e = ps VU.! ei,
            concatable e [a, b, c, d]
        ]
  -- 0.9s
  print $ snd $ head res
