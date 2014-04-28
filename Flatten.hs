flatten' :: [[a]] -> [a]
flatten' as = foldl (\acc x -> acc ++ x)  [] as