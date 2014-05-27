import Data.Map as Map

fromListWith' :: (Ord a) => ([a] -> [a] -> [a]) -> [a] -> Map.Map a a
fromListWith' f xs = fromListWith'' f xs Map.empty	
                      where fromListWith'' _ [(), _] acc = acc
                      	    fromListWith'' f ((k,v):ys) acc
                              | Map.member k acc = Map.insert k $ f oldV v $ acc
                              | otherwise = Map.insert k v acc 
                                where oldV = Map.lookup v acc -- TODO: `Option.get` for Haskell


-- > Map.fromListWith (\num1 num2 -> num1 ++ ", " ++ num2) [("1","1"), ("2","2"), ("1","33")]
-- fromList [("1","33, 1"),("2","2")]