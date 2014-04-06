--LYAH
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "max of empty list"
maximum' [x] = x
maximum' (x:xs)
   | x > maxTail = x
   | otherwise = maxTail
   where maxTail = maximum' xs 

-- maximum' [2,5,1]
-- 2 > maximum' [5,1]
-- 2 > (5 > maximum' [1])
-- 2 > (5 > 1) 
-- 2 > 5 
-- 5