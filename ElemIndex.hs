elemIndex' :: (Eq a) => a -> [a] -> Maybe Int
elemIndex' _ []     = Nothing
elemIndex' y xs = elemIndex'' y xs 0 
                  where elemIndex'' _ [] _ = Nothing
                        elemIndex'' z (x:xs) i
                          | x == z    = Just i
                          | otherwise = elemIndex'' z xs (i+1)