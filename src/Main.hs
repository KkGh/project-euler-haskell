import Data.Time
import qualified P040 as P

main = do
  s <- getCurrentTime

  P.main

  e <- getCurrentTime
  print $ diffUTCTime e s
