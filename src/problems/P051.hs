module P051 (main) where

import Data.List
import qualified Data.Vector.Unboxed as VU
import Util (combinationsRep, permutationsMultiset, primeSieveV, readInt)

main = do
  -- 同じ桁を置換すると8つの素数ファミリーが得られる素数は？

  let ps = primeSieveV (10 ^ 6)

  -- n桁で*がm個ある文字列を生成する。数字はn-m個含まれる。
  -- 3桁で*が2個の場合、"**d","*d*","d**"を生成する。
  let genStr' n m =
        filter valid $
          concat
            [ permutationsMultiset $ ('*', m) : gs
              | xs <- combinationsRep (n - m) ['0' .. '9'],
                let gs = map (\g -> (head g, length g)) $ group xs
            ]
        where
          valid s = head s /= '0' && last s `elem` "1379"
  let genStr n = concat [genStr' n m | m <- [1 .. n - 1]]

  -- '*'に0~9を割り当てて、素数となる数を返す。
  let genPrimes str =
        [ str'
          | d <- ['0' .. '9'],
            let str' = map (\c -> if c == '*' then d else c) str,
            head str' /= '0',
            ps VU.! readInt str'
        ]

  -- 2.5s
  print $ head $ snd $ head [(s, ps) | n <- [1 .. 6], s <- genStr n, let ps = genPrimes s, length ps == 8]
