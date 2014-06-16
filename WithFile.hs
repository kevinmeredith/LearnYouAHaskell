import System.IO

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path iomode f = do
  handle <- openFile path iomode
  result <- f handle
  hClose handle
  return result