isSuffixOf' :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf' [] _ = False
isSuffixOf' _ [] = False