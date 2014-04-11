-- It takes a function and two lists as parameters 
-- and then joins the two lists by applying the 
-- function between corresponding elements.
zipWith' :: [a] -> [a] -> (a -> a -> a) -> [a]
zipWith' xs ys f  
  | (null xs) || (null ys) = []
  | otherwise              = f (head xs) (head ys) : zipWith' (tail xs) (tail ys) f