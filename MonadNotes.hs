--(>>=) :: (Monad m) => m a -> (a -> m b) -> m b

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

class Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail msg = error msg

instance Monad [] where
   return x = [x]
   xs >>= f = concat (map f xs)    
   fail _ = []

class Monad m => MonadPlus m where
   mzero :: m a
   mplus :: m a -> m a -> m a 

instance MonadPlus [] where
  mzero = []
  mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero   

-- Monad Laws

--1. return x >>= f equals f x
--2. m >>= return equals f x
--3. (m >>= f) >== g equals m >>= (f >>= g)

-- Composing Monads
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> mc)
f <=< g = (\x -> g x >>= f)

newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')


instance Monad ((->) r) where
  return x = \_ -> x
  h >>= f = \w -> f (h w) w  

-- State Monad
--s -> (a, s)

newtype State s a = State { runState :: s -> (a, s)}

instance Monad (State s) where 
  return x = State $ \x -> (x, s)
  (State h) >>= f = State $ \s -> let (a, newState) = h s
                                      (State g) = f a 
                                  in g newState


  -- Monad m => m a -> (a -> m b) -> m b 
  instance (Error e) => Monad (Either e) where
    return x = Right x
    Right x >>= f = f x
    Left err >>= f = Left err
    fail msg = Left (strMsg msg)

-- It's worth examining what the type of >>= would be if it were only implemented for State
(>>=) :: State s a -> (a -> State s b) -> State s b

liftM :: (Monad m)  => (a -> b) -> m a -> m b
fmap :: (Functor f) => (a -> b) -> f a -> f b

--implement liftM with >>=
liftM :: (Monad m)  => (a -> b) -> m a -> m b
liftM f m = m >>= (\x -> return(f x))

liftM :: (Monad m)  => (a -> b) -> m a -> m b
liftM f m = do
  x <- m
  return (f x)

-- Applicative's <*> can be implemented with Monad
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b  

ap :: (Monad m) => m (a -> b) -> m a -> m b  
ap f m = m >>= (\x -> )

ap :: (Monad m) => m (a -> b) -> m a -> m b  
ap f m = do
  g <- f
  m2 <- m
  return (g m2)

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a1 a2 = f <$> a1 <*> a2

join :: (Monad m) => m (m a) -> m a
join = do
  m <- mm
  m

-- >>= is always the same as 
-- join (fmap f m)

