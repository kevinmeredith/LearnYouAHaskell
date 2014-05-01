-- any and all take a predicate and then check if any or all the elements
-- in a list satisfy the predicate, respectively. Usually we use 
-- these two functions instead of mapping over a list and then doing and or or.
any' :: (a -> Bool) -> [a] -> Bool
any' _ []     = False
any' f (x:xs) = if (f x) then True else any' f xs

all' :: (a -> Bool) -> [a] -> Bool
all' f ys = foldl (\acc x -> if (f x) then acc else False) True ys

allq' :: (a -> Bool) -> [a] -> Bool
allq' _ []     = True
allq' f (x:xs) 
   | f x = allq' f xs
   | otherwise = False