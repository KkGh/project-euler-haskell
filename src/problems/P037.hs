module P037 (main) where

import Data.List
import qualified Data.Vector.Unboxed as VU
import GHC.Plugins
import Util (digitsToInt, intToDigits, primeSieveV)

trueIndices :: VU.Vector Bool -> [Int]
trueIndices = VU.ifoldr' (\i b acc -> if b then i : acc else acc) []

main = do
  -- 11個の素数の合計は？ただし、素数の inits, tails が全て素数となる
  let psv = primeSieveV $ 10 ^ 6
      isPrime n = psv VU.! n
      ps = trueIndices psv
  let res = filter f $ dropWhile (< 10) ps
        where
          f p = do
            let truncates = inits (intToDigits p) ++ tails (intToDigits p)
            let ns = map digitsToInt $ filter notNull truncates
            all isPrime ns

  -- 25ms
  print $ sum res
