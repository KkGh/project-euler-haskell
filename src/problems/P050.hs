module P050 (main) where

import Data.List
import qualified Data.Set as S
import Util (maximumOn, primeSieve)

main = do
  let n = 1000000
  let ps = primeSieve n
  let psSet = S.fromAscList ps

  -- 合計が1000000以下となる連続素数列
  let conPrimes = is ++ ts
        where
          is = takeWhile (\(_, total) -> total <= n) $ map (\xs -> (xs, sum xs)) $ inits ps
          ts = map (\xs -> (xs, sum xs)) $ concatMap (tails . fst) is

  -- 合計が素数となるもの
  let conPrimes' = filter (\(_, total) -> total `S.member` psSet) conPrimes

  -- 列が最長となるもの
  let res = maximumOn (length . fst) conPrimes'

  -- 0.22s
  print $ snd res
