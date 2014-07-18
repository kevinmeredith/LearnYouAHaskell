myAction :: IO String
myAction = do
  a <- getLine
  b <- getLine
  return $ a ++ b

 -- re-write as Applicative

myActionApplicative :: IO String
   pure (++) <*> getLine <*> getLine -- from LYAH