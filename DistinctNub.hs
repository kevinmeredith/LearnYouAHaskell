-- only include distinct elements
nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' xs = nubHelper xs []

nubHelper :: (Eq a) => [a] -> [a] -> [a]
nubHelper []     acc = acc
nubHelper (y:ys) acc = if(contains' y acc)  
                         then nubHelper ys acc 
                         else nubHelper ys (acc ++ [y])
                         --where contains match acc = 
                         --	foldl (\as x -> if(y == x) then True else as) False acc

contains' :: (Eq a) => a -> [a] -> Bool
contains' _ [] = False
contains' x (y:ys)
   | x == y = True
   | otherwise = contains' x ys
