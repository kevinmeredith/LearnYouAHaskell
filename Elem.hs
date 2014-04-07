-- true if item is in list, else false
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = if x == a then True else elem' a xs