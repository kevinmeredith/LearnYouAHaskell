elemIndices' :: (Eq a) => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' y ys = elemIndices'' y ys 0
                     where elemIndices'' _ [] _ = []
                           elemIndices'' z (x:xs) acc
                            | z ==  x   = acc : elemIndices'' z xs (acc + 1)
                            | otherwise = elemIndices'' z xs (acc + 1)