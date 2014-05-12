find' :: (a -> Bool) -> [a] -> Maybe a
find' _ []     = Nothing
find' f (x:xs) 
  | f x       = Just x
  | otherwise = find' f xs