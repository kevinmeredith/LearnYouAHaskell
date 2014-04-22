-- Say we have a list of numbers and we want to turn them all into negative numbers
negate' :: (Num a, Ord a) => [a] -> [a]
negate' ys = map (\x -> abs' x) ys

abs' :: (Num a, Ord a) => a -> a
abs' x 
  | x <= 0 = x
  | otherwise = -x