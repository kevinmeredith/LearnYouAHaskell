-- Now we're going to make a program that continuously reads 
-- a line and prints out the same line with the words reversed. 
-- The program's execution will stop when we input a blank line.

main = do
	putStrLn "Enter a word to be reversed"
	word <- getLine
	putStrLn