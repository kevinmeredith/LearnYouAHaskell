import System.Environment

--instance Functor IO where
--  fmap f action = do
--    result <- action
--    return (f result)

main = do line <- getLine     
          let line' = reverse line
          putStrLn $ "You said " ++ line' ++ " backwards."
          putStrLn $ "Yes, you really said " ++ line' ++ " backwards."