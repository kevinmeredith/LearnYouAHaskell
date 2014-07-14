-- Pathological Functor Case

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
	fmap f (CJust x a) = CJust (x+1) (f a)
	fmap f CNothing    = CNothing