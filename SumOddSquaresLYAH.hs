-- LYAH. Definitely cleaner. Thinking of Scala Days 2013 key note, I'm thinking 
-- out loud whether it's better to break out each of the lists...
sumOddSquatesLt10k :: Integer
sumOddSquatesLt10k = sum ( takeWhile (< 10000) (map (^2) (filter odd [1..])) )
