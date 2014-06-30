finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen = 
	let (value, newGen) = random gen
	    (restOfList, finalGen) = finiteRandoms (n-1) newGen
	in (value:restOfList, finalGen)