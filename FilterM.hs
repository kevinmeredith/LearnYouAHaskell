-- http://stackoverflow.com/questions/25476248/powerset-function-1-liner

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' f xs = filterM'' xs []
                where filterM'' []      acc = return acc
                      filterM'' (y:ys)  acc = if (r) then filterM'' ys (y : acc) 
                                              else        filterM'' ys acc
                      do 
                      	r <- f x
                      