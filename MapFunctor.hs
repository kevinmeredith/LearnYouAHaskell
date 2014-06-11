-- Try figuring out how `Map k` is made an instance 
-- of Functor

import Data.Map

instance functor MyMap where
	fmap f Map.empty = Map.empty
	fmap f (Map k v) = Map.insert $ Map.singleton k (f v) $ fmap 