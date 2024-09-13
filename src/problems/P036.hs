module P036 (main) where

import Text.Printf
import Util (digits)

intToBitString :: Int -> String
intToBitString = printf "%b"

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

main = do
  -- 2進数でも10進数でも回文な数の1Mまでの和

  let isPal10 x = isPalindrome $ digits x
  let isPal2 x = isPalindrome $ intToBitString x
  let result = filter (\x -> isPal10 x && isPal2 x) [1 .. 10 ^ 6 - 1]
  -- 0.17s
  print $ sum result
