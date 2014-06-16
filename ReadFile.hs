-- LYAH readFile

import System.IO

main = do
   contents <- readFile "girlfriend.txt"
   putStrLn contents