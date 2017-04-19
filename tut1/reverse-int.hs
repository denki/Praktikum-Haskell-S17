myReverseInt :: [Int] -> [Int]
myReverseInt = h [] where
  h xs []     = xs
  h xs (y:ys) = h (y:xs) ys
