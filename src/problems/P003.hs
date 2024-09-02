module P003 (main) where

-- 最大素因数
main = do
  print $ last $ primeFactors 600851475143

-- 素因数
primeFactors :: Int -> [Int]
primeFactors n = go n 2
  where
    -- 試し割り法：2からNの平方根まで順に割っていく
    go n f
      | f * f > n = [n]
      | n `mod` f == 0 = f : go (n `div` f) f
      | otherwise = go n (f + 1)
