import Control.Monad.Writer
import Data.Monoid

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
	mempty = DiffList (\xs -> [] ++ xs)
	(DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

finalCountDown :: Int -> Writer (DiffList String) ()  
finalCountDown 0 = do  
    tell (toDiffList ["0"])  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell (toDiffList [show x])  
