-- LYAH
import Control.Monad
import Data.Char

main = forever $ do
     putStr "Give me some input: "
     x <- getLine
     putStrLn $ map toUpper x