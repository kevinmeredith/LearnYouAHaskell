last' :: [a] -> Maybe a
last' [] = Nothing
last' ys = Just (foldl (\acc x -> acc) (head (reverse ys)) ys)