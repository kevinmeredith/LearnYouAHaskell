--checks if uppercase
isUpperCase' :: Char -> Bool
isUpperCase' = (`elem` ['A'..'Z'])