module P091 (main) where

toCoord :: Int -> Int -> (Int, Int)
toCoord cols i = (i `mod` cols, i `div` cols)

distSq :: (Num a) => (a, a) -> (a, a) -> a
distSq (x1, y1) (x2, y2) = (x2 - x1) ^ 2 + (y2 - y1) ^ 2

main = do
  -- 原点o、p=(x1,y1), q=(x2,y2) からなる三角形を全て生成する。
  -- ただし、0 <= x1,y1,x2,y2 <= 50
  let cols = 51
      rows = 51
      len = cols * rows
      triangles = [(toCoord cols pi, toCoord cols qi) | pi <- [1 .. len - 1], qi <- [pi + 1 .. len - 1]]
  let isRightTriangle p@(x1, y1) q@(x2, y2) =
        let lensSq = [x1 ^ 2 + y1 ^ 2, x2 ^ 2 + y2 ^ 2, distSq p q]
         in maximum lensSq == sum lensSq - maximum lensSq
  -- 0.2s
  print $ length . filter (uncurry isRightTriangle) $ triangles
