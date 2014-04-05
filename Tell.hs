tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element - " ++ show x
tell (x:y:[]) = "has 2 elements" ++ show x ++ " and " ++ show y
tell (x:y:_) = "has many! first 2 are: " ++ show x ++ " and " ++ show y