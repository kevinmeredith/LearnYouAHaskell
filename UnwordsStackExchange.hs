import Data.List (intersperse)

unwords' :: [String] -> String
unwords' = concat . intersperse " "