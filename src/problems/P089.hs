module P089 (main) where

import Util

main = do
  ls <- readLines "src/data/0089_roman.txt"

  -- subtraction rule:
  --   4=IIII=IV
  --   9=VIIII=IX
  --   40=XXXX=XL
  --   90=LXXXX=XC
  --   400=CCCC=CD
  --   900=DCCCC=CM
  let romanToNum str = sum $ go str
        where
          go [] = []
          go ('M' : rest) = 1000 : go rest
          go ('C' : 'M' : rest) = 900 : go rest
          go ('D' : rest) = 500 : go rest
          go ('C' : 'D' : rest) = 400 : go rest
          go ('C' : rest) = 100 : go rest
          go ('X' : 'C' : rest) = 90 : go rest
          go ('L' : rest) = 50 : go rest
          go ('X' : 'L' : rest) = 40 : go rest
          go ('X' : rest) = 10 : go rest
          go ('I' : 'X' : rest) = 9 : go rest
          go ('V' : rest) = 5 : go rest
          go ('I' : 'V' : rest) = 4 : go rest
          go ('I' : rest) = 1 : go rest
          go _ = error ""
  let numToRoman n = do
        (["", "M", "MM", "MMM", "MMMM"] !! (n `div` 1000))
          ++ (["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"] !! ((n `div` 100) `mod` 10))
          ++ (["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"] !! ((n `div` 10) `mod` 10))
          ++ (["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"] !! (n `mod` 10))
  let res = [(s, n, r, length s - length r) | s <- ls, let n = romanToNum s, let r = numToRoman n]
  -- 4ms
  print $ sum $ map (\(_, _, _, d) -> d) res
