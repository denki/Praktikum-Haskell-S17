fib :: Int -> Int
fib = fib' 0 1

fib' :: Int -> Int -> Int -> Int
fib' x _ 0 = x
fib' x y n = fib' y (x + y) (n - 1)
