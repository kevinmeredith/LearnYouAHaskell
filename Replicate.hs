-- replicate count item_to_replicate
replicate' :: Int -> a -> [a]
replicate' n x 
  | n <= 0    = []
  | otherwise = [x] ++ replicate' (n - 1) x