-- LYAH started the exercise, but I implemented the Monad

import Data.Ratio

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show  

join :: Prob (Prob a) -> Prob a
join mm = do
   m <- mm
   m 

-- without do notation
join' :: Prob (Prob a) -> Prob a
join' mm = mm >>= (\x -> x)

instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs  

-- fmap :: Functor f => (a -> b) -> f a -> f b

instance Monad Prob where
	return x  = Prob [(x, 1%1)]
	p >>= f   = join (fmap f p)

-- return :: Monad m => a -> m a
-- (>>=) :: Monad m  => m a -> (a -> m b) -> m b