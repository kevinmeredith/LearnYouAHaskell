--http://codereview.stackexchange.com/questions/49031/haskells-group-function
group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' (x:xs) = (x: prefix) : group' remainder
     where (prefix, remainder) = span (== x) xs