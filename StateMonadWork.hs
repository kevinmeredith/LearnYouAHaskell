-- from http://stackoverflow.com/a/1957379/409976
import Control.Monad.State

len2State :: String -> State Int Bool
len2State s = return ((length s) == 2)

len2 :: String -> Bool
len2 s = ((length s) == 2)

len2State' :: String -> (Int -> (Bool, Int))
len2State' s i = (len2 s, i)

convert :: Bool -> (Int -> (Bool, Int))
convert r d = (r, d)

len2State'' :: String -> (Int -> (Bool, Int))
len2State'' s = convert (len2 s)

useState :: (Int -> Bool) -> Int -> (Bool, Int)
useState f d = (f d, d)

len :: String -> Int -> Bool
len s i = (length s) == i

lenState :: String -> (Int -> (Bool, Int))
lenState s = useState (len s) 

chainStates :: (Int -> (result1, Int)) -> (result1 -> (Int -> (result2, Int))) -> (Int -> (result2, Int))
chainStates prev f d = let (r, d') = prev d
                       in f r d'

extractState :: Int -> (Int, Int)
extractState d = (d, d)

chained :: String -> (Int -> (Bool, Int))
chained str = chainStates extractState     $ \state1 ->
              let check1 = (len str state1) in
              chainStates (overwriteState (
              	if check1
              		then state1
              		else state1 * 2))       $ \ _ ->
              chainStates extractState      $ \state2 -> 
              let check2 = (len str state2) in
              convert (check1 || check2)