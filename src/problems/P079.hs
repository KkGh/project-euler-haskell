module P079 (main) where

import Data.List
import Data.Ord
import GHC.Plugins
import Util (groupOn, intToDigits)

main = do
  let keys = [319, 680, 180, 690, 129, 620, 762, 689, 762, 318, 368, 710, 720, 710, 629, 168, 160, 689, 716, 731, 736, 729, 316, 729, 729, 710, 769, 290, 719, 680, 318, 389, 162, 289, 162, 718, 729, 319, 790, 680, 890, 362, 319, 760, 316, 729, 380, 319, 728, 716]
  let ords = nubSort $ concatMap (\r -> let [a, b, c] = intToDigits r in [(a, b), (a, c), (b, c)]) $ nubSort keys
  let groups = sortOn (Down . length) $ groupOn fst ords
  -- <1ms
  putStrLn $ concatMap (show . fst . head) groups ++ "0"