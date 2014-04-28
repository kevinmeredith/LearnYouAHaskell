--pattern matching on list of lists
foo :: [[a]] -> [a] -> [a]
foo [[]]  _ = []
foo (x:xs) i
  | null xs   = x 
  | otherwise = x ++ i ++ foo xs i