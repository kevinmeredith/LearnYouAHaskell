 --class (Functor f) => Applicative f where
 --  pure :: a -> f a 
 --  (<*>) :: f (a -> b) -> f a -> f b

-- [(*3), (+1)] <*> [1,2] where zipWith-ish behavior is used instead of <*>
-- instance Applicative ZipList where  
--        pure x = ZipList (repeat x)  
--        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

import Control.Applicative

getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]