module P031 (main) where

main = do
  -- 1,2,5,10,20,50,100,200の8種類のコインがある。
  -- 合計が200となる組み合わせは何通り？
  let coins = [1, 2, 5, 10, 20, 50, 100, 200]

  -- 残りの額nを、n以下かつ選択したコイン以下のコインのみで構築する
  let f _ [1] = 1 -- 使えるコインが1だけになった
      f 0 _ = 1 -- 残りが0になった
      f n cs = sum $! [f (n - c) (filter (<= c) cs) | c <- filter (<= n) cs]

  let res = f 200 coins
  -- 10ms
  print res
