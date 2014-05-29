-- LYAH
 class Eq' a where
   (==) :: a -> a -> Bool
   (/=) :: a -> a -> Bool
   x == y = not (x Main./= y)
   x /= y = not (x Main.== y)

data TrafficLight = Red | Yellow | Green   

instance Eq TrafficLight where
	Red == Red = True
	Green == Green = True
	Yellow == Yellow = True
	_ == _ = False

instance Show TrafficLight where
   show Red = "Red Light"	
   show Yellow = "Yellow Light"	
   show Green = "Green Light"	

-- Need (Eq m) to ensure that m's can be compared for equality!
instance (Eq m) => Eq (Maybe m) where
	Just x == Just y = x == y
	Nothing == Nothing = True
	_ == _ = False