-- union also acts like a function on sets. It returns the union of two lists, no dupes.
-- note - dupes are only removed from the second list - 

union' :: (Eq a) => [a] -> [a] -> [a]
union' xs ys = xs ++ union'' ys xs
                where union'' [] _     = []
                      union'' (a:as) first = if (a `elem` first) then union'' as first else a : union'' as (a:first)
