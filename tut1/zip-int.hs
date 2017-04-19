myZipInt :: [Int] -> [Int] -> [(Int, Int)]
myZipInt [] _ = []
myZipInt _ [] = []
myZipInt (x:xs) (y:ys) = (x, y) : myZipInt xs ys
