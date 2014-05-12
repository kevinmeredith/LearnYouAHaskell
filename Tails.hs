tails :: [a] -> [[a]]
tails []         = []
tails (x:[])     = [x] : [] : []    -- is `tails []` better here?
tails xxs@(_:xs) = xxs : tails xs
