product' :: (Num a) => [a] -> a
product' [] = 0
product' ys = foldl1 (\acc x -> x*acc) ys