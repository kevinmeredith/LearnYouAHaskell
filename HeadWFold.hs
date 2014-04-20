 head' :: [a] -> Maybe a
 head' [] = Nothing
 head' ys = Just (foldl (\acc x-> acc) (head ys) ys)