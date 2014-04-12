filter' :: [a] -> (a -> Bool) -> [a]
filter' [] _     = []
filter' (x:xs) f 
 | f x       = x : filter' xs f
 | otherwise = filter' xs f