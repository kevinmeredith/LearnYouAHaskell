type Birds = Int
type Pole = (Birds, Birds)

landLeft1 :: Birds -> Pole -> Pole
landLeft1 n (left, right) = (left + n,right)

landRight1 :: Birds -> Pole -> Pole
landRight1 n (left, right) = (left,right + n)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs((left + n) - right) < 4 = Just (left+n, right)
  | otherwise                   = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs((left) - (right + n)) < 4 = Just (left, right+n)  
  | otherwise                     = Nothing

-- Make Pierre slip and fall
banana :: Pole -> Maybe Pole
banana _ = Nothing

-- *Main Control.Applicative Control.Monad> landLeft 2 (0,0) >>= landRight 2
-- Just (2,2)

-- *Main Control.Applicative Control.Monad> Nothing >>= landRight 5
-- Nothing

foo :: Maybe Pole
foo = do
  start <- return (0,0)
  first <- landLeft 4 start
  second <- landRight 5 first
  landLeft 3 second

makePierreFall :: Maybe Pole
makePierreFall = do
  start <- return (0,0)
  first <- landLeft 3 start
  Nothing
  second <- landRight 2 first
  landLeft 2 second