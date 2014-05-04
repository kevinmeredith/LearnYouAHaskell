--  it drops all the elements while the predicate is true. 
-- Once predicate equates to False, it returns the rest of the list.
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []     = []
dropWhile' f (x:xs) = if (f x) then dropWhile' f xs else x:xs