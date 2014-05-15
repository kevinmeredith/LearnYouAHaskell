insert' :: (Ord a) => a -> [a] -> [a]
insert' x []     = [x]
insert' x yys@(y:ys) = if x <= y then x : yys else y : insert' x ys