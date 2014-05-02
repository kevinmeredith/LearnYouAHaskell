-- or is like and, only it returns True if 
-- any of the boolean values in a list is True.
or' :: [Bool] -> Bool
or' []     = False
or' (x:xs) = x || or' xs 