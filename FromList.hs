import qualified Data.Map as Map

fromList :: (Ord k) => [(k, v)] -> Map.Map k v
fromList []           = Map.empty
fromList ((k,v) : xs) = Map.insert k v $ fromList xs

-- LYAH uses a fold, so I'm re-implementing
fromListFold :: (Ord k) => [(k, v)] -> Map.Map k v
fromListFold = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty