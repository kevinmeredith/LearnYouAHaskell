oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum_' :: Integer
oddSquareSum_' =
	let oddSquares = filter odd $ map (^2) [1..]
		belowLimit = takeWhile (<10000) oddSquares
	in sum belowLimit
