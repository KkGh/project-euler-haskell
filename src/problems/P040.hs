module P040 (main) where

main = do
  -- 20ms
  print $ read [xs !! 1] * read [xs !! 10] * read [xs !! 100] * read [xs !! 1000] * read [xs !! 10000] * read [xs !! 100000] * read [xs !! 1000000]
  where
    xs = concatMap show [0 ..] :: String
