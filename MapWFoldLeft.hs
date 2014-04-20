map' :: (a -> b) -> [a] -> [b]
map' f ys = foldl (\acc x -> acc ++ [f x]) [] ys

----elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys