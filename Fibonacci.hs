-- Fibonacci
-- 0 1 1 2 3 5 8

fib :: (Integral a) => a -> a
fib n
 | n == 0 = 0
 | n == 1 = 1
 | otherwise = fib(n-1) + fib(n-2)