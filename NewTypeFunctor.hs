class Functor f where 
	fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
	fmap :: (a -> b) -> Maybe a -> Maybe b

-- Now what if we wanted to make the tuple an instance of Functor 
-- in such a way that when we fmap a function over a tuple, it gets 
-- applied to the first component of the tuple?

newtype MaybeTuple = MaybeTuple { getMaybe :: Maybe Int}