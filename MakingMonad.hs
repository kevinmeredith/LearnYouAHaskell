-- LYAH

import Data.Ratio

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show  

instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs  

-- fmap :: Functor f => (a -> b) -> f a -> f b

instance Monad Prob where
	return x = Prob [(x, 1%1)]
	(Prob xs) >>= f  = Prob $ -- TODO!

-- return :: Monad m => a -> m a
-- (>>=) :: Monad m  => m a -> (a -> m b) -> m b