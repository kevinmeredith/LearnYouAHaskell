-- If that number is even, we divide it by two. If it's odd, 
-- we multiply it by 3 and then add 1 to that. 

odd' :: (Integral a) => a -> Bool
odd' x = x `mod` 2 == 1

collatz :: (Integral a) => a -> [a]
collatz x 
 | odd' x    = f x : collatz (f x)
 | otherwise = g x : collatz (g x)
     where f y = y*3 + 1
           g y = y `div` 2   -- per http://stackoverflow.com/a/23050795/409976