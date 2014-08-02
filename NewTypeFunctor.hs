--class Functor f where 
--	fmap :: (a -> b) -> f a -> f b

--instance Functor Maybe where
--	fmap :: (a -> b) -> Maybe a -> Maybe b

-- Now what if we wanted to make the tuple an instance of Functor 
-- in such a way that when we fmap a function over a tuple, it gets 
-- applied to the first component of the tuple?

import Control.Applicative

--LYAH
newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair c) where
	fmap f (Pair (x, y)) = Pair (f x, y)