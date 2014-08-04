--Ugly Alternative to Monads

type Bird = Int
type Pole = (Bird,Bird)

routine :: Maybe Pole 
routine = case landLeft 1 (0,0) of 
	Nothing -> Nothing
	Just pole1 -> case landRight 4 pole1 of
		Nothing -> Nothing
		Just pole 2 -> case landLeft 2 pole2 of 
			Nothing -> Nothing
			Just pole3 -> landLeft 1 pole3