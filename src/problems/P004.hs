module P004 (main) where

-- 3桁の整数の積のうち、最大の回文数
main = do
  -- 0.04s
  print $
    maximum $
      [ n
        | x <- [100 .. 999],
          y <- [x, x - 1 .. 100],
          let n = x * y,
          isPalindrome . show $ x * y
      ]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = head xs == last xs && isPalindrome (init . tail $ xs)
