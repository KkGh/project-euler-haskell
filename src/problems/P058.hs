module P058 (main) where

import Safe (findJust)
import Util (cumsum, fi, isPrime, isSquareInt)

main = do
  -- ウラムの螺旋
  -- 対角線上の数の素数率が10％を下回るのは？
  -- top right:    [3, 13, 31, 57] => 4*n^2 - 2*n + 1 = (2n+1 + √(6n))(2n+1 - √(6n))
  -- top left:     [5, 17, 37, 65] => 4*n^2       + 1 = (2n+1 + 2√n)(2n+1 - 2√n)
  -- bottom left:  [7, 21, 43, 73] => 4*n^2 + 2*n + 1 = (2n+1 + √(2n))(2n+1 - √(2n))
  -- bottom right: [9, 25, 49, 81] => 4*n^2 + 4*n + 1

  -- spiralの中心をn=0とする
  let spiralTR n = 4 * n ^ 2 - 2 * n + 1
  let spiralTL n = 4 * n ^ 2 + 1
  let spiralBL n = 4 * n ^ 2 + 2 * n + 1
  -- let spiralBR n = 4 * n ^ 2 + 4 * n + 1
  let primesOnCorner 1 = [3, 5, 7]
      primesOnCorner n = map fst $ filter snd [(tr, isPrimeTR), (tl, isPrimeTL), (bl, isPrimeBL)]
        where
          tr = spiralTR n
          tl = spiralTL n
          bl = spiralBL n
          isPrimeTR = not (isSquareInt (6 * n)) && isPrime tr
          isPrimeTL = not (isSquareInt n) && isPrime tl
          isPrimeBL = not (isSquareInt (2 * n)) && isPrime bl
      primeRatio (c, n) = fi c / fi (4 * n + 1)

  let cumCounts = cumsum $ map (length . primesOnCorner) [1 ..]
  let res = findJust (\(c, n) -> primeRatio (c, n) < 0.1) $ zip cumCounts [1 ..]
  -- 1.48s
  print $ 2 * snd res + 1
