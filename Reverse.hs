--"ABC" -> "CBA" 
reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = last xs : reverse' (init xs)