take' :: (Integral i, Ord i) => [a] -> i -> [a]
take' xs n 
  | null xs || n < 0 = []
  | n == 0           = []
  | otherwise        = [head xs] ++ take' (tail xs) (n-1)