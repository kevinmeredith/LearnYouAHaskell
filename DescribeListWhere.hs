--LYAH
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
     where what [] = "empty"
           what [x] = "singleton list"
           what xs = "a longer list"