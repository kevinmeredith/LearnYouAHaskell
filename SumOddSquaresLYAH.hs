-- LYAH. Definitely cleaner. Thinking of Scala Days 2013 key note, I'm thinking 
-- out loud whether it's better to break out each of the lists...
sumOddSquaresLt10k :: Integer
sumOddSquaresLt10k = sum ( takeWhile (< 10000) (map (^2) (filter odd [1..])) )

--using list comprehension
sumOddSquaresLt10kListComp :: Integer
sumOddSquaresLt10kListComp = sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])