-- check out http://codereview.stackexchange.com/a/49695/31595
-- I had a redundant case for `unwords' [] = []`
unwords' :: [String] -> String
unwords' (x:xs)     
  | null xs   = x
  | otherwise = x ++ " " ++ unwords' xs

-- ghci> unwords ["hey","there","mate"]  
-- "hey there mate"  