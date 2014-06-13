-- LYAH testing `getContents`
import Data.Char

main = do
   contents <- getContents
   putStr (map toUpper contents)