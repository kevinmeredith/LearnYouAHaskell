partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f []     = ([], [])
partition' f ys = partition'' f ys [] []
                       where partition'' f [] as bs = (as, bs)
                             partition'' f (x:xs) as bs 
                               | f x       = partition'' f xs (as ++ [x]) bs
                               | otherwise = partition'' f xs as (bs ++ [x])
