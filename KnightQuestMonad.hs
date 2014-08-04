--LYAH
import Control.Monad

type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
	(c',r') <- [(c+2, r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
	           ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
	           ]
	guard (c' `elem` [1..8] && r' `elem` [1..8])
	return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
	first <- moveKnight start
	second <- moveKnight first
	moveKnight second

in3Bind :: KnightPos -> [KnightPos]	
in3Bind start = return start >>= moveKnight >>= moveKnight >>= moveKnight

arriveIn3 :: KnightPos -> KnightPos -> Bool
arriveIn3 start end = end `elem` in3 start

moveKnightWithMove :: KnightPos -> [(KnightPos, KnightPos)]
moveKnightWithMove (c,r) = do
	(c',r') <- [(c+2, r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
	           ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
	           ]
	guard (c' `elem` [1..8] && r' `elem` [1..8])
	return ((c,r), (c', r'))