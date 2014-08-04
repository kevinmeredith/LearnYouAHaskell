fooWithout :: Maybe String 
fooWithout = Just 3 >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

fooDo :: Maybe String
fooDo = do
   x <- Just 3
   y <- Just "!"
   Just (show x ++ y)

doNotationEx :: Maybe String
doNotationEx = do
	x <- Just 3
	y <- Nothing
	Just (show x ++ y)

--Just 9 >>= (\x -> Just (x > 8))  

doCompare :: Maybe Bool
doCompare = do
	x <- Just 9
	Just (x > 8)