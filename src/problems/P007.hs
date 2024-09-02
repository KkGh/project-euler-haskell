module P007 (main) where
import Util

main = do
  -- 10001st Prime
  let ps = primeSieve 1000000
  print $ ps !! 10000
