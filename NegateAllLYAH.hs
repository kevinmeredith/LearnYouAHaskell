--negate all using function composition
negateAll' :: (Num a, Ord a) => [a] -> [a]
negateAll' xs = map (negate . abs) xs