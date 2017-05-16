import Data.List (foldl')

averageBad :: Fractional a => [a] -> a
averageBad xs = sum xs / fromIntegral (length xs)

average :: Fractional a => [a] -> a
average = (\ (s, c) -> s / c) . foldl' (\(s, c) a -> (s + a, c + 1)) (0,0)
