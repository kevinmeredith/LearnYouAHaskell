intersperse' :: [a] -> a -> [a]
intersperse' ys i = foldl (\acc x -> acc ++ [x] ++ [i]) [] ys

is' :: [a] -> a -> [a]
is' [] _ = []
is' (x:xs) i
  | null xs = [x]
  | otherwise = x : (i : (is' xs i))