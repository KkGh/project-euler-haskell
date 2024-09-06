module P046 (main) where

import Data.List
import Data.Maybe
import Safe
import Util (isPrime, primeSieve)

main = do
  -- 奇数の合成数集合 = 奇数集合 - 素数集合
  let ps = primeSieve 10001
  let oddCompos = [3, 5 .. 10001] \\ ps
  let f o = case find (\(p, _) -> isPrime p) $ takeWhile (\(p, _) -> p > 0) $ map (\n -> (o - 2 * n ^ 2, n)) [1 ..] of
        Just (p, n) -> Just (o, p, n)
        Nothing -> Nothing
  -- 30ms
  print $ findJust (isNothing . f) oddCompos
