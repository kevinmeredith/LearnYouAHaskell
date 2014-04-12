map' :: [a] -> (a -> b) -> [b]
map' [] _     = []
map' (x:xs) f = f x : map' xs f 