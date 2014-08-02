class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

-- Monoid Laws
-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

-- instance Monoid [a] where
--	mempty = []
--	mappend = (++)

--newtype Product a = Product { getProduct :: a }
--  deriving (Eq, Ord, Read, Show, Bounded)

-- instance Monoid Ordering where
--	mempty = EQ
--	LT `mappend` _ = LT
--	EQ `mappend` y = y
--	GT `mappend` _ = GT