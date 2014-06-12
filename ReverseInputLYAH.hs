-- Now we're going to make a program that continuously reads 
-- a line and prints out the same line with the words reversed. 
-- The program's execution will stop when we input a blank line.

-- LYAH

main = do
	line <- getLine
	if null line
		then return ()
        else do
        	putStrLn $ reverseWords line
        	main

reverseWords :: String -> String
reverseWords = unwords. map reverse . words

--Prelude Data.Char> words "FOO BAR"
--["FOO","BAR"]
--Prelude Data.Char> unwords . map reverse . words $ "FOO BAR"
--"OOF RAB"
