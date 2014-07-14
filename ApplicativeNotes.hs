 class (Functor f) => Applicative f where
   pure :: a -> f a 
   (<*>) :: f (a -> b) -> f a -> f b

 instance Applicative Maybe where
   pure = Just
   Nothing <*> _ = Nothing
   (Just f) <*> something = fmap f something

-- Prelude Control.Applicative> :t (<$>)
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- f <$> x = fmap f x 

 instance Applicative [] where 
 	pure x = [x]
 	fs <*> xs = [f x | f <- fs, x <- xs]