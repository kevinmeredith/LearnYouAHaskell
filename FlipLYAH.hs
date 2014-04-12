-- Flip simply takes a function and returns a function that is like 
-- our original function, only the first two arguments are flipped.
-- LYAH code
flip' :: (a -> b -> c) -> (b -> a -> c) 
flip' f = g
  where g x y = f y x 

-- *Main> let foo a b = a - b
-- *Main> let flipped = flip foo
-- *Main> foo 100 200
-- -100
-- *Main> flipped 100 200
--100

-- Even easier to write
flipSimpler' :: (a -> b -> c) -> b -> a -> c
flipSimpler' f x y = f y x
