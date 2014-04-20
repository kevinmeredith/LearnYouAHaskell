-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?
-- *Main> length (takeWhile (< 1000) (scanl (+) 0 (map sqrt [1..]) )) + 1
-- 132

-- But I should've used `scanl1` since the first acc value, 0, is incorrectly included

-- *Main> length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]) )) + 1
-- 131