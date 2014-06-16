-- view, add and delete tasks
import System.Environment
import System.IO
import Data.List

data Action = Add | View | Delete

main = do
	args <- getArgs
	progName <- getProgName
	let maybeAction = validArgs args
  	execute maybeAction

execute :: (Maybe Action) -> IO a
execute Just Add    = print "Add not supported"
execute Just View   = view
execute Just Delete = print "Delete not supported"
execute None        = print "invalid user input"

view :: IO a
view = do
	withFile "todo_file.txt" ReadMode (\handle -> do 
		contents <- hGetContents handle
		putStr contents)

validArgs :: [String] -> Maybe Action
validArgs (x:[]) = isValidAction x
validArgs _      = None

isValidAction :: String -> Maybe Action
isValidAction x
  | x == "view"   = Just View
  | x == "add"    = Just Add
  | x == "delete" = Just Delete
  | otherwise     = None