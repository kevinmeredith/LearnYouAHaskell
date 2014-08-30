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

-- return :: Monad m => a -> m a
-- (>>=) :: Monad m  => m a -> (a -> m b) -> m b

foldingFunction :: (a, Rational) -> [(a, Rational)] -> [(a, Rational)]
foldingFunction x acc = (fst x, (snd x * ))

flatten :: Prob (Prob a) -> Prob a
flatten mm = mm >>= (\m -> map (rduce) (getProb m))     
        where rduce = foldr foldingFn [] (snd $ getProb)    --(Prob Char) -> [(Char, Ratio)]
              foldingFn x acc = (fst x, ((snd x * myRatio) :: Rational)) : acc
              myRatio = (snd $ ) 

-- motivates how flattening should work
thisSituation :: Prob (Prob Char)
thisSituation = Prob
  [( Prob [('a', 1%2),('b',1%2)], 1%4)
  ,( Prob [('c', 1%2),('d',1%2)], 3%4)
  ]

-- desired end result:
-- Prob [('a', 1%8), ('b', 1%8), (c,3%8), (d,3%8)]