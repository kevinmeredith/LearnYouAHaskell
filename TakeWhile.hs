takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []     = []
takeWhile' f (x:xs) = if f x then x : takeWhile' f xs else []

--span is kind of like takeWhile, only it returns a pair of lists. 
-- The first list contains everything the resulting list from 
-- takeWhile would contain if it were called with the same predicate 
-- and the same list. The second list contains the part of the list that would have been dropped.
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ []     = ([], [])
span' f (x:xs) = span'' f (x:xs) []
                   where span'' f (y:ys) acc
                          | null ys   = (acc, [])
                          | otherwise = if f y then span'' f ys (acc ++ [y]) else (acc, y:ys)