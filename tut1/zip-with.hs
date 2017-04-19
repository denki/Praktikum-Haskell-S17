myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _     = []
myZipWith _ _ []     = []
myZipWith f (x:xs) (y:ys) = x `f` y : myZipWith f xs ys

myZip :: [a] -> [b] -> [(a,b)]
myZip = myZipWith (,)
