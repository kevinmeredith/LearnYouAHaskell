--unlines takes a list of strings and joins them together using a '\n'.
unlines' :: [String] -> String
unlines' ys = foldr (\x acc -> x ++ "\n" ++ acc) [] ys

-- ["hey", "world"]
-- "hey\nworld\n"

-- unlines ["first line", "second line", "third line"]  
-- "first line\nsecond line\nthird line\n" 