sum' :: (Num a) => [a] -> a
sum' as = foldl (+) 0 as