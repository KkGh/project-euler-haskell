module P100 (main) where

import Data.Array
import Data.List
import Data.Maybe
import Util (fi)

-- 二次方程式の解の公式により、2つの解 (+,-) を返す。
quadraticFomula :: (Floating b) => b -> b -> b -> (b, b)
quadraticFomula a b c = ((-b + r) / d, (-b - r) / d)
  where
    r = sqrt (b ^ 2 - 4 * a * c)
    d = 2 * a

main = do
  -- 21 discs = 15 blue + 6 red から 2つを取り出したとき、2つともblueになる確率は 15/21 * 14/20 = 1/2
  -- 次に確率が50％になるのは 85 blue + 35 red, 85/120 * 84/119 = 1/2
  -- 10^12 disksを超えるときの blue discsはいくつか？

  let memo = array (0, 1000) [(i, a i) | i <- [0 .. 1000]] :: Array Integer Integer
      a 0 = 0
      a 1 = 1
      a n = 6 * memo ! (n - 1) - memo ! (n - 2) - 2
  let n = fromJust $ find (> 10 ^ 12) memo
  -- <1ms
  print $ floor $ fst $ quadraticFomula 2 (-2) (fi $ -n ^ 2 + n)
