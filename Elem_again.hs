elem' :: (Eq a ) => a -> [a] -> Bool
elem' _ []     = False
elem' x (y:ys) 
  | x == y    = True
  | otherwise = elem' x ys

notElem' :: (Eq a) => a -> [a] -> Bool
notElem' x ys = not $ elem' x ys