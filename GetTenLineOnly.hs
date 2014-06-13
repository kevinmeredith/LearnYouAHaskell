-- LYAH 10-length lines only
main = do
  xs <- getContents
  putStrLn (shortLineOnly xs)

-- TODO
shortLineOnly :: String -> String
shortLineOnly xs 
