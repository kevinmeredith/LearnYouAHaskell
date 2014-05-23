-- check if map is empty
import qualified Data.Map as Map

-- TODO: I don't like using `size` per Daniel Spiewak's talk. I'd 
-- rather check if it's equal to Map.empty, but not sure how to
-- pattern match a Map.Map k v.
null' :: (Ord k) => Map.Map k v -> Bool
null' xs
 | Map.size xs == 0 = True
 | otherwise        = False

-- null' = foldr (\(k,v) acc -> False) True 