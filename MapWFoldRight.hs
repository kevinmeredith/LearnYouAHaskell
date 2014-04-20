map' :: (a -> b) -> [a] -> [b]
map' f ys = foldr (\x acc -> (f x) : acc) [] ys -- I appended to `acc`, but LYAH prepends to it, plus signature of lambda

-- [1 2 3]
-- foldr goes from right to left