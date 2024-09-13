module P049 (main) where

import Data.List
import qualified Data.Set as S
import GHC.Utils.Misc
import Safe
import Util (allEqual, combinations, digitsToInt, digits, primeSieve)

-- |
--
-- >>> permutationsInt 231
-- [231,321,132,312,123,213]
permutationsInt n = map digitsToInt $ permutations $ digits n

diffs [] = []
diffs [_] = []
diffs (x : ys@(y : _)) = y - x : diffs ys

main = do
  -- 互いにpermutationとなる3つの素数 p,q,r は？ただし、p,q,rは等差数列。
  let ps = dropWhile (< 1000) $ primeSieve 9999
  let psSet = S.fromList ps
  let isPrime n = n `S.member` psSet
  let isArithSeq ns = allEqual $ diffs ns

  let permSet =
        filter (\xs -> length xs >= 3) $
          nubSort $
            map (filter isPrime . nubSort . permutationsInt) ps
  let res = concatMap (filter isArithSeq . combinations 3) permSet
  -- 10ms
  print $ concatMap show $ findJust (\[p, _, _] -> p /= 1487) res
