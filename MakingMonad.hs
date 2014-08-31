-- LYAH started the exercise, but I implemented the Monad

import Data.Ratio

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show  

join' :: Prob (Prob a) -> Prob a
join' mm = do
   m <- mm
   m 

-- without do notation
join'' :: Prob (Prob a) -> Prob a
join'' mm = mm >>= (\x -> x)

instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs  

-- fmap :: Functor f => (a -> b) -> f a -> f b

instance Monad Prob where
	return x = Prob [(x, 1%1)]
	p >>= f  = join' (fmap f p)    -- matches type, but not conceptually 
	                               -- correct for this probability example

-- motivates how flattening should work
thisSituation :: Prob (Prob Char)
thisSituation = Prob
  [( Prob [('a', 1%2),('b',1%2)], 1%4)
  ,( Prob [('c', 1%2),('d',1%2)], 3%4)
  ]

-- start: Prob (Prob Char)
-- end:   Prob Char

flatten :: Prob (Prob a) -> Prob a
flatten = Prob . convert . getProb 

-- calling getProb on thisSituation returns: [(Prob Char, Rational)]
-- then we need to convert to [(Char, Rational)]
convert :: [(Prob a, Rational)] -> [(a, Rational)]
convert xs = concat $ map f xs

f :: (Prob a, Rational) -> [(a, Rational)]
f (p, r) = map (mult r) (getProb p)

mult :: Rational -> (a, Rational) -> (a, Rational)
mult r (x, y) = (x, r*y)

-- getProb :: Prob a -> [(a, Rational)]


-- return :: Monad m => a -> m a
-- (>>=) :: Monad m  => m a -> (a -> m b) -> m b
