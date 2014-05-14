-- http://codereview.stackexchange.com/a/49508/31595

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
 | f x       = x:rest
 | otherwise = rest
    where rest = filter' f xs