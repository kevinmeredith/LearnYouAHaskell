import System.Environment
import System.IO
import System.Directory

main = do (fileName:_) <- getArgs
          fileExists <- doesFileExist fileName
          if fileExists
             then do contents <- readFile fileName
                     putStrLn $ "it has" ++ show (length (lines contents))
             else do putStrLn "The file doesn't exist!"