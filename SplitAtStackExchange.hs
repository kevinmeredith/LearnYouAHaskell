--http://codereview.stackexchange.com/a/48910/31595
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' 0 ys     = ([], ys)
splitAt' n []     = ([], [])
splitAt' n (y:ys) = ((y:a), b)
   where (a, b) = splitAt' (n - 1) ys