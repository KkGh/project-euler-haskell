module P087 (main) where

import qualified Data.Set as S
import Util (isqrt, primeSieve)

main = do
  -- 50Mを超えない三つ組み
  let limit = 50 * 10 ^ 6
  let ps = primeSieve (isqrt limit)
  let squares = takeWhile (< limit) $ map (^ 2) ps
  let cubes = takeWhile (< limit) $ map (^ 3) ps
  let fourths = takeWhile (< limit) $ map (^ 4) ps
  let result = [s + c + f | s <- squares, c <- cubes, f <- fourths, s + c + f < limit]
  -- 1.5s
  print $ S.size $ S.fromList result
