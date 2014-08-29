import Control.Monad

f :: a -> Maybe a
f = \x -> Just x

g :: a -> [a]
g = \x -> [x]

foo :: Monad m => a -> m c
foo x = f <=< g x