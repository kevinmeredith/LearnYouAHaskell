--Let's find the largest number under 100,000 
--that's divisible by 3829. (avoiding filter & last for now)
findLargestDiv' :: (Integral a) => a -> a -> Maybe a
findLargestDiv' n count
   | n < 0 = Nothing
   | otherwise = z
   where y = last (filter (divByN n) [1..count])
         z = Just(y)
   	     
divByN :: (Integral a) => a -> a -> Bool
divByN x y = (mod y x) == 0

-- TODO: ask StackOverflow!!!
--filterDivByN :: Integer -> Integer -> [Integer]
--filterDivByN x n = filter ((mod x) == 0) [1..n]

-- TODO: find out how to do
-- filter ((mod 3829) == 0) [1..100000]