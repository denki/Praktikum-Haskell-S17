myMapInt :: (Int -> Int) -> [Int] -> [Int]
myMapInt f []     = []
myMapInt f (x:xs) = f x : myMapInt f xs
