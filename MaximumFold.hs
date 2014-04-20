maximum' :: (Ord a) => [a] -> a
maximum' ys = foldl1 (\acc x -> if(x > acc) then x else acc) ys