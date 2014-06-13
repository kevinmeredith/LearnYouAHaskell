-- read characters from input until ' '
main = do
	x <- getChar
	if x == ' '
		then return ()
		else do 
             putChar x
             main