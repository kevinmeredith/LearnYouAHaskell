scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' _ acc []       = [acc]
scanr' f acc (y:ys) = (scanr' f (f y acc) ys) ++ [acc]