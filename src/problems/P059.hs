module P059 (main) where

import Control.Monad
import Data.Bits
import Data.Char
import Data.Foldable
import Data.List
import Data.List.Split

-- | リストから長さnの全ての重複順列を生成する。
--
-- >>> repetitions 2 ['a', 'b', 'c']
-- ["aa","ab","ac","ba","bb","bc","ca","cb","cc"]
repetitions :: (Integral a) => a -> [b] -> [[b]]
repetitions 0 _ = [[]]
repetitions n xs = [x : ys | x <- xs, ys <- repetitions (n - 1) xs]

main = do
  -- 3桁の小文字英字からなるキー
  contents <- readFile "src/data/0059_cipher.txt"
  let cipher = concatMap (map (\s -> read s :: Int) . splitOn ",") $ lines contents

  let keys = map (map ord) $ repetitions 3 ['a' .. 'z']
  for_ keys $ \key -> do
    -- 3桁のキーを使って復号
    let decrypted = zipWith xor cipher (cycle key)
    let isSuccess = all (\d -> d >= 32 && d <= 122) decrypted && ("the" `isInfixOf` map chr decrypted)
    when isSuccess $ do
      -- print $ map chr decrypted
      -- 10ms
      print $ sum decrypted
