-- delete : deletes first occurrence of item
delete' :: (Eq a) => a -> [a] -> [a]
delete' y xs = delete'' y xs []
                 where delete'' _ [] _ = []
                       delete'' d (a:as) acc
                         | a == d    = reverse acc ++ as
                         | otherwise = delete'' d as (a:acc)
