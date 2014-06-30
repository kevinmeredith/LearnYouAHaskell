import System.Environment
import System.IO
import System.IO.Error
import Control.Exception

main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e 
    | isDoesNotExistError e = 
         case ioeGetFileName e of Just path -> putStrLn $ "whoops. file does not exist at:" ++ path
                                  Nothing   -> putStrLn $ "whoops. file does not exist at unknown location."
    | otherwise = ioError e                                  