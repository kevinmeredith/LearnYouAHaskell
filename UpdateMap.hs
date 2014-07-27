import Data.Map as Map

updateMap :: (Ord k, Num v) => k -> v -> Map k v -> Map k v
updateMap k v map = if member k map then Map.adjust (+ v) k map
	                else Map.insert k v map 