-- view, add and delete tasks
-- borrows delete from previous LYAH exercise
import System.Environment
import System.Directory
import System.IO
import Data.List

data Action = Add | View | Delete

main = do
	args <- getArgs
	progName <- getProgName
	let maybeAction = validArgs args
  	executeTodo maybeAction

executeTodo :: (Maybe Action) -> IO ()
executeTodo (Just Add)    = addTodo
executeTodo (Just View)   = viewTodo
executeTodo (Just Delete) = deleteTodo
executeTodo Nothing       = print "invalid user input"

viewTodo :: IO ()
viewTodo = do
	withFile "todo_file.txt" ReadMode (\handle -> do 
		contents <- hGetContents handle
		putStr contents)
	return ()

addTodo :: IO ()
addTodo = do
   todoItem <- getLine
   appendFile "todo_file.txt" (todoItem ++ "\n")	

deleteTodo :: IO () 
deleteTodo = do        
    handle <- openFile "todo_file.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks     
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"     
    numberString <- getLine     
    let number = read numberString     
        newTodoItems = delete (todoTasks !! number) todoTasks     
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "todo_file.txt"  
    renameFile tempName "todo_file.txt"  

validArgs :: [String] -> Maybe Action
validArgs (x:[]) = isValidAction x
validArgs _      = Nothing

isValidAction :: String -> Maybe Action
isValidAction x
  | x == "view"   = Just View
  | x == "add"    = Just Add
  | x == "delete" = Just Delete
  | otherwise     = Nothing