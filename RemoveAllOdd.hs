let removeAllOddNumbers xs = [x | x <- xs, x `mod` 2 == 0]

-- test
let listOfLists = [ [1,2], [3], [4,3,2] ]

-- should return [[2],[],[4,2]]
map removeAllOddNumbers listOfLists
