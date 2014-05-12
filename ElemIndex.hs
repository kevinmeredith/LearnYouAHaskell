elemIndex' :: (a -> Bool) -> [a] -> Maybe Int
elemIndex' _ []     = Nothing
elemIndex' f xs = elemIndex'' f xs 0 
                  where elemIndex'' _ [] _ = Nothing
                        elemIndex'' f (x:xs) i
                          | f x       = Just i
                          | otherwise = elemIndex'' f xs (i+1)