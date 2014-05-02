--and takes a list of boolean values and returns True only if 
-- all the values in the list are True.
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs