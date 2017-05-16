{-# LANGUAGE BangPatterns #-}

sumWorst :: Int -> Int
sumWorst 0 = 0
sumWorst n = n + sumWorst (n - 1)

sumBad :: Int -> Int
sumBad = go 0
  where
    go :: Int -> Int -> Int
    go acc 0 = acc
    go acc n = go (acc + n) (n - 1)

sumGood :: Int -> Int
sumGood = go 0
  where
    go :: Int -> Int -> Int
    go acc 0 = acc
    go 0   n = go n (n - 1)
    go acc n = go (acc + n) (n - 1)

sumBest :: Int -> Int
sumBest = go 0
  where
    go :: Int -> Int -> Int
    go acc 0 = acc
    go acc n = acc `seq` go (acc + n) (n - 1)

sumBest' :: Int -> Int
sumBest' = flip go 0
  where
    go :: Int -> Int -> Int
    go 0 acc = acc
    go n acc = go (n - 1) $! (acc + n)

sumBang :: Int -> Int
sumBang = go 0
  where
    go :: Int -> Int -> Int
    go !acc 0 = acc
    go !acc n = go (acc + n) (n - 1)
