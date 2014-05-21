findKey' :: (Eq k) => k -> [(k,v)] -> v  
findKey' k xs = snd $ head $ filter ((== k) . fst) xs

findKeyMaybe' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKeyMaybe' k xs 
  | null $ findAttempt = Nothing
  | otherwise          = Just $ snd $ head $ findAttempt
  where findAttempt = filter ((== k) . fst) xs

findKeyFold :: (Eq k) => k -> [(k,v)] -> Maybe v
findKeyFold k = foldr (\x acc -> if (fst x == k) then Just $ snd x else acc) Nothing

--phoneBook =   
--    [("betty","555-2938")  
--    ,("bonnie","452-2928")  
--    ,("patsy","493-2928")  
--    ,("lucille","205-2928")  
--    ,("wendy","939-8282")  
--    ,("penny","853-2492")  
--    ]  