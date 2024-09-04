module P032 (main) where

import Data.Containers.ListUtils
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Util

hasDuplicate xs = length xs /= S.size (S.fromList xs)

main = do
  -- a * b = c で、 a/b/cが1~9のパンデジタル数になるcの合計は？
  -- ただし、a < b < c
  --   0は出現しない
  --   a,b,cの桁数の合計は9
  --   aは99まで (99*100=9900 9桁, 100*101=10100 11桁)
  let go a = mapMaybe (\b -> if isPandigital a b then Just (a, b, a * b) else Nothing) [a + 1 .. 9876]
      isPandigital a b =
        0 `notElem` as
          && 0 `notElem` bs
          && 0 `notElem` cs
          && totalLen == 9
          && not (hasDuplicate as)
          && not (hasDuplicate bs)
          && not (hasDuplicate cs)
          && null (as `intersect` bs)
          && null (as `intersect` cs)
          && null (bs `intersect` cs)
        where
          c = a * b
          as = intToDigits a
          bs = intToDigits b
          cs = intToDigits c
          totalLen = digitCount a + digitCount b + digitCount c
  let res = concatMap go [2 .. 99]
  let prods = nubInt $ map trd3 res
  -- 0.21s
  print $ sum prods