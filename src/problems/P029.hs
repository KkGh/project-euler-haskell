module P029 (main) where

import qualified Data.Set as S

main = do
  -- a^b
  let res = S.fromList [a ^ b | a <- [2 .. 100], b <- [2 .. 100]]
  -- 10ms
  print $ S.size res
