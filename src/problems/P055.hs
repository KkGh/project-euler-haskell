module P055 (main) where

import Util

-- | 整数を逆順にする。
--
-- >>> reverseInt 1230
-- 321
reverseInt = go 0
  where
    go acc 0 = acc
    go acc x = go (acc * 10 + x `mod` 10) (x `div` 10)

isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = head xs == last xs && isPalindrome (init . tail $ xs)

isPalindromeInt n = isPalindrome $ digits n

-- 自然数nがリクレル数かどうかを返す。
-- リクレル数：桁を前後反転させたものと自身との和を求める操作を繰り返したとき、回文数にならない自然数。
isLychrel n maxIt = not $ any isPalindromeInt $ take maxIt xs
  where
    xs = tail $ iterate (\n -> n + reverseInt n) n

main = do
  -- リクレル数
  -- 2桁までの自然数はいずれ回文数になるため、リクレル数でない。
  -- 50ms
  print $ length $ [n | n <- [100 .. 10000], isLychrel n 50]
