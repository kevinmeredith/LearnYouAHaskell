-- replicate count item_to_replicate
-- added `Ord` per LYAH
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x 
  | n <= 0    = []
  | otherwise = [x] ++ replicate' (n - 1) x