-- implementing maximum with an unfavorable 'b' argument
maximum' :: (Ord a) => [a] -> a -> a
maximum' [] b = error "empty list!"
maximum' [x] b = if x > b then x else b
maximum' (x:xs) b = if x > b then maximum' xs x else maximum' xs b 
