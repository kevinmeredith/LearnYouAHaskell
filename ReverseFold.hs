reverse' :: [a] -> [a]
reverse' ys = foldl (\acc x -> x : acc) [] ys