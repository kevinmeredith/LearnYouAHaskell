pushToTop :: [a] -> Int -> Maybe [a]
pushToTop [] _   = Just []
pushToTop xs i 
  | i >= length xs || i < 0 = Nothing
  | i == 0                  = Just xs
  | otherwise               = Just $ newTop : rest
                              where zipped = zip [0..] xs
                              	    newTop = xs !! i
                                    rest   = (map (\x -> snd x) . filter (\x -> fst x /= i)) zipped