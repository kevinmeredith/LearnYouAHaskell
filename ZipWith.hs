-- It takes a function and two lists as parameters 
-- and then joins the two lists by applying the 
-- function between corresponding elements.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys 
  | (null xs) || (null ys) = []
  | otherwise              = f (head xs) (head ys) : zipWith' f (tail xs) (tail ys)