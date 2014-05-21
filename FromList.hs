import qualified Data.Map as Map

fromList :: (Ord k) => [(k, v)] -> Map.Map k v
fromList []           = Map.empty
fromList ((k,v) : xs) = Map.insert k v $ fromList xs