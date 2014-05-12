-- does list contain sub-list?
isInfixOf' :: (Eq a) => [a] -> [a] -> Bool
isInfixOf'  [] _      = False
isInfixOf'  _ []      = False
isInfixOf' yys@(y:ys) xxs@(x:xs)
  | null ys && x == y = True
  | x == y            = isInfixOf' xs ys
  | otherwise         = isInfixOf' xs yys


-- isInfixOf' "cat" "hey there cat burglar!" --> TRUE
-- isInfixOf' "CAt" "hey there cat burglar!" --> FALSE