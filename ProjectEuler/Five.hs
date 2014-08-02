--Problem 5.
--2520 is the smallest number that can be divided by each of the numbers 
-- from 1 to 10 without any remainder.
--What is the smallest positive number that is evenly divisible by 
-- all of the numbers from 1 to 20?

isDivByAll :: Int -> Int -> Bool
isDivByAll n num = foldr (\e acc -> if (num `mod` e == 0) then acc else False) True [1..n]

findSmallestDiv :: Int -> Int
findSmallestDiv n = go 1
	where go num = if (isDivByAll n num) then num else go (num+1)

-- result
--*Main Control.Applicative> findSmallestDiv 10
--2520
--*Main Control.Applicative> findSmallestDiv 20
--232792560