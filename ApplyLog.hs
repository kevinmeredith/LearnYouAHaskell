import Data.Monoid

applyLog1 :: (a, String) -> (a -> (b,String)) -> (b, String)
applyLog1 (x,log) f = let (y,newLog) = f x in (y, log ++ newLog)

-- only useful for Lists....
-- applyLog :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])

-- use `mappend` since ByteString and List are Monoids
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y, log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _       = ("beer", Sum 30)