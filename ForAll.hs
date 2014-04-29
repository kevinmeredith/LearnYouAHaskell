forall' :: (a -> Bool) -> [a] -> Maybe Bool
forall' _ [] = Nothing
forall' f ys = foldl (\acc x -> if(not $ f x) then (Just False) else acc) (Just True) ys