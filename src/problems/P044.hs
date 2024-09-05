module P044 (main) where

import qualified Data.Set as S
import Util (pairs)

pentagonal :: (Integral a) => a -> a
pentagonal n = n * (3 * n - 1) `div` 2

main = do
  let pentagons = map pentagonal [1 .. 10000]
  let set = S.fromList pentagons
  let isPentagonal n = n `S.member` set
  let res =
        [ ((pj, pk), diff)
          | (pj, pk) <- pairs pentagons,
            let diff = pk - pj,
            isPentagonal (pj + pk) && isPentagonal diff
        ]
  -- 0.6s
  print $ snd $ head res
