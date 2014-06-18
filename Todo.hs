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

bump :: IO ()
bump = do
  handle <- openFile "todo_file.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
      putStrLn "Pick which item you want bumped to the top:"
      putStrLn $ unlines numberedTasks
      numberString <- getLine
      let number = read numberString
          newTodoItems = pushToTop todoTasks number
      


-- As an exercise, you can try implementing a bump function that will take a file and a task number 
-- and return an I/O action that bumps that task to the top of the to-do list.
pushToTop :: [a] -> Int -> Maybe [a]
pushToTop [] _   = Just []
pushToTop xs i 
  | i >= length xs || i < 0 = Nothing
  | i == 0                  = Just xs
  | otherwise               = Just $ newTop : rest
                              where zipped = zip [0..] xs
                                    newTop = xs !! i
                                    rest   = (map (\x -> snd x) . filter (\x -> fst x /= i)) zipped

