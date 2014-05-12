findIndex' :: (a -> Bool) -> [a] -> Maybe Int
findIndex' _ [] = Nothing
findIndex' f ys = findIndex'' f ys 0
                    where findIndex'' _ [] _ = Nothing
                    	  findIndex'' g (x:xs) i
                           | g x       = Just i
                           | otherwise = findIndex'' g xs (i+1)
