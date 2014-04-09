compareTo100 :: (Num a, Ord a) => a -> Ordering
compareTo100 x 
 | x < 100   = GT
 | x > 100   = LT
 | otherwise = EQ