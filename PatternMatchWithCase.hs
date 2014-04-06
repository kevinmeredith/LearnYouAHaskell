-- LYAH
head' :: [a] -> a
head' xs = case xs of [] -> error "no head!"
                      (x:_) -> x