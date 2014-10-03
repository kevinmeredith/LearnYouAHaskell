-- foldr :: (a -> b -> b) -> b -> [a] -> b
max' :: Ord a => [a] -> Maybe a
max' []     = Nothing
max' (x:xs) = Just $ foldr (\y acc -> if (y > acc) then y else acc) x xs