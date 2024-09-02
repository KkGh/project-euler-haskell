module P019 (main) where

import Data.Time

main = do
  print $
    length $
      [ day
        | y <- [1901 .. 2000],
          m <- [1 .. 12],
          let day = fromGregorian y m 1,
          show (dayOfWeek day) == "Sunday"
      ]
