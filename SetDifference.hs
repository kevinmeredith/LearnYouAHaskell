-- \\ is the list difference function. It acts like a set difference, basically. 
-- For every element in the right-hand list, 
-- it removes a matching element in the left one.

setDifference :: (Eq a) => [a] -> [a] -> [a]
setDifference xs excludes = filter (not . (`elem` excludes)) xs