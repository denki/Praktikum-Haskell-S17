data Tree a = Node a [Tree a] deriving Show

spinalTree :: Int -> Tree Int
spinalTree 0 = Node 0 []
spinalTree i = Node i [Node i [], spinalTree (pred i), Node i []]

height :: Tree a -> Int
height (Node _ []) = 1
height (Node _ ts) = 1 + maximum (map height ts)

yield :: Tree a -> [a]
yield (Node x []) = [x]
yield (Node _ ts) = concatMap yield ts

-- based on Data.Tree.flatten
yieldFast :: Tree a -> [a]
yieldFast t = go t []
  where
    go (Node x []) xs = x : xs
    go (Node _ ts) xs = foldr go xs ts
