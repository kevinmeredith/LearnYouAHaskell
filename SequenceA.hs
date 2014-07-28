-- Let's try implementing a function that takes a list of applicatives and 
-- returns an applicative that has a list as its result value. 
import Control.Applicative
--sequenceA' :: (Applicative f) => [f a] -> f [a] 
sequenceAMaybe' :: (Num a) =>  [Maybe a] -> Maybe [a] 
sequenceAMaybe' []  = pure []
sequenceAMaybe' xs = pure $ [extract' x | x <- xs ]

extract' :: (Num a) => Maybe a -> a 
extract' x  = case x of 
	  Just y  -> y
	  Nothing -> 0

-- LYAH
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA []     = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

-- substitution model
-- sequenceA [Just 1, Just 2]
-- (:) <$> Just 1 <*> sequenceA [Just 2]
-- (:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])
-- reduces to
--(:) <$> Just 1 <*> Just [2]
-- Just [1,2] -- finished

-- with fold
sequenceAFold' :: (Applicative f) => [f a] -> f [a]
sequenceAFold' = foldr (\x acc -> (:) <$> x <*> acc) $ pure []

--LYAH
sequenceAFoldLYAH' :: (Applicative f) => [f a] -> f [a]
sequenceAFoldLYAH' = foldr (liftA2 (:)) $ pure []

-- Using `sequenceA` on list of functions
--*Main Control.Applicative> sequenceA [(+3), (*2)] $ 3
--[6,6]

-- substitution
-- (:) <$> (+3) <*> sequenceA [(*2)]
-- (:) <$> (+3) <*> ((:) <$> (*2) <*> sequenceA [])
-- (:) <$> (+3) <*> ((:) <$> (*2) <*> pure [])
-- (:) <$> (+3) <*> [(*2)]
-- [(+3), (*2)]