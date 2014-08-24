--http://stackoverflow.com/questions/1956518/state-monad-sequences-of-random-numbers-and-monadic-code?rq=1

chainStates :: (Int -> (result1, Int)) -> (result1 -> (Int -> (result2, Int))) -> (Int -> (result2, Int))