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

-- for all starting numbers between 1 and 100, how many chains 
-- have a length greater than 15? 

collatzChainLength :: (Integral a) => a -> Int
collatzChainLength x = length (takeWhile (/= 1) (collatz x) )

--*Main> let xs = map collatzChainLength [1..100]
--*Main> let ys = map (> 15) xs
--*Main> let convertBool x = if x then 1 else 0
--*Main> sum (map convertBool ys)
--60
