 -- all products of [2,5,10] and [8,10,11] using Applicative

import Control.Applicative

result = [(*2), (*5), (*10)] <*> [8, 10, 11]

-- If we wanted all possible products of those two lists 
-- that are more than 50, we'd just do:

filtered = filter (> 50) result