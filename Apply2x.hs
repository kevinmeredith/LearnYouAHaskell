-- apply function twice
apply2x :: (a -> a) -> a -> a
apply2x f x = f (f x)