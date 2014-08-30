import Control.Monad

f :: a -> Maybe a
f = \x -> Just x

g :: a -> [a]
g = \x -> [x]

--(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
-- in the above signature for <=<, only a single Monad can be used
foo :: Monad m => a -> m c
foo x = f <=< g x