-- fold using the beginning of the list. Error on empty
-- can't use `b` type since how we would know about it?
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f ys = foldl f (head ys) (tail ys) -- updated per Lee Duhem's answer on StackOverflow
                                           -- http://stackoverflow.com/q/23183935/409976

foldlOption :: (a -> a -> a) -> [a] -> Maybe a
foldlOption f [] = Nothing
foldlOption f ys = Just (foldl f (head ys) (tail ys))