-- If that number is even, we divide it by two. If it's odd, 
-- we multiply it by 3 and then add 1 to that. 

odd' :: (Integral a) => a -> Bool
odd' x = x `mod` 2 == 1

even' :: (Integral a) => a -> Bool
even' x = x `mod` 2 == 0

-- updating to end at 1 & use even'
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x 
 | odd' x  = x : collatz (f x)
 | even' x = x : collatz (g x)
     where f y = y*3 + 1
           g y = y `div` 2   -- per http://stackoverflow.com/a/23050795/409976

-- for all starting numbers between 1 and 100, how many chains 
-- have a length greater than 15? 

-- let cs = map collatz [1..100]
-- let ls = map length cs
-- let gt15 = map (> 15) ls
-- let convertBool x = if x then 1 else 0
-- sum (map convertBool gt15)

