-- fold using the beginning of the list. Error on empty
-- can't use `b` type since how we would know about it?
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f ys = foldl f (head ys) ys

foldlOption :: (a -> a -> a) -> [a] -> Maybe a
foldlOption f [] = Nothing
foldlOption f ys = Just (foldl f (head ys) ys)