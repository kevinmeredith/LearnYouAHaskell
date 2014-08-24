-- State Monad explanation on StackOverflow 
-- http://stackoverflow.com/questions/25464680/understanding-state-monad
-- s -> (a, s)

upd :: Int -> (Int, Int)
upd s = let s' = s + 1 in (s', s')

compose :: (s -> (a, s))
        -> (a -> (s -> (b,s)))
        -> (s -> (b, s))
compose f f' = \s ->  let (a, s') = f s
                          (b, s'') = f' a s'
                      in  (b, s'')

-- author: Kevin
twoUnique :: Int -> ((Int, Int), Int)
twoUnique i = let (a, _)  = upd i
                  (b, _)  = upd a
              in  ((a,b), b)

twoUniqueComposed :: Int -> ((Int, Int), Int)
twoUniqueComposed = compose upd (\a s -> let (a', s') = upd s in ((a, a'), s'))

-- compare compose to (>>=)
compose f f' = \s -> let (a, s')  = f s
                         (b, s'') = f' a s'
                     in  (b, s'')

(>>=) (State f) f' = State $ \s -> let (a, s')  = f s
                                       (b, s'') = runState (f' a) s'
                                   in  (b, s'')