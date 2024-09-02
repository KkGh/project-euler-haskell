module P009 (main) where

main = do
  print $ a * b * c
  where
    (a, b, c) =
      head
        [ (a, b, 1000 - a - b)
          | a <- [2 .. 1000],
            b <- [a + 1 .. 1000],
            a * a + b * b == (1000 - a - b) ^ 2
        ]
