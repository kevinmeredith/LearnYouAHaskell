isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' [] _ = False
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) 
  | null xs && x == y = True
  | x == y            = isPrefixOf' xs ys
  | otherwise         = False

isSuffixOf' :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf' xs ys = isPrefixOf' (reverse' xs) (reverse' ys)

--isSuffixOf "foobar" "bar" == isPrefixOf "raboof" "rab"

reverse' :: [a] -> [a]
reverse' ys = foldl (\acc x -> x : acc) [] ys