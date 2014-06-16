--LYAH
import System.IO

main = do
	withFile' "girlfriend.txt" ReadMode (\handle -> do 
		contents <- hGetContents handle
		putStr contents)

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path iomode f = do
  handle <- openFile path iomode
  result <- f handle
  hClose handle
  return result