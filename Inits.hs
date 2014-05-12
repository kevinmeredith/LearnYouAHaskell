inits :: [a] -> [[a]]
inits [] = []
inits ys = inits' ys [] 
           where  inits' [] acc     = acc : []
    	          inits' (x:xs) acc = acc : inits' xs (acc ++ [x])

--[] : [1] : inits' [2,3] (1:[])

--inits "foo"
--["", "f", "fo", "foo"]

--inits [1,2,3]
--inits' [1,2,3] []
--[] : inits' [2,3] ([] ++ [1])
--[] : [1] : inits' [3] ([1] ++ [2])
--[] : [1] : inits' [3] ([1] ++ [2]) []
--[] : [1] : [1,2] : [1,2,3] : []