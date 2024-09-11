module P080 (main) where

import Util (fi, isSquareInt)

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = zipWith f [0 ..]

-- | 開平法
-- 自然数の平方根を10進数展開し、整数部と小数点以下をタプルにして返す。
--
-- >>> let (x,y) = extractSqrt 2 in (x, take 10 y)
-- ([1],[4,1,4,2,1,3,5,6,2,3])
extractSqrt :: (Integral a) => a -> ([a], [a])
extractSqrt n = splitAt (length xs) $ go 0 0 $ xs ++ [0, 0 ..]
  where
    -- nを2桁ずつブロックに分ける
    xs = reverse $ f n
      where
        f 0 = []
        f x = let (d, m) = x `divMod` 100 in m : f d

    -- 筆算によって平方根の各桁を計算する。qは左の筆算の和、dは余りrと積pの差を表す
    go _ _ [] = []
    go q d (x : xr) = a : go (q * 10 + a * 2) (r - p) xr
      where
        -- rを超えない桁aとその時の積pを探す
        r = d * 100 + x
        (a, p) = last $ takeWhile ((<= r) . snd) $ imap (\a x -> (fi a, (q * 10 + x) * x)) [0 .. 9]

extractSqrt' n floatLimit = let (xs, ys) = extractSqrt n in (xs, take floatLimit ys)

main = do
  -- 6ms
  print $
    sum
      [ sum xs + sum (take (100 - length xs) ys)
        | n <- [1 .. 100],
          not . isSquareInt $ n,
          let (xs, ys) = extractSqrt' (fi n) 100
      ]
