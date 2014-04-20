filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f ys = foldl (\acc x -> if(f x) then (acc ++ [x]) else acc) [] ys