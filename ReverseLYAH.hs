--LYAH
--note that this version is better than mine (at least for performance)
-- since it doesn't use standard libraries (last, init)
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]