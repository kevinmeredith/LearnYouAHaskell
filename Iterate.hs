iterate' :: (a -> a) -> a -> [a]
iterate' f x = y : (iterate' f y)
    where y = f x