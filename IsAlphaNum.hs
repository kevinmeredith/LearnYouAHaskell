-- check if String is alphanumeric only
import Data.Char (isAlphaNum)

isAlphaNum' :: String -> Bool
isAlphaNum' = all isAlphaNum

isAlphaNumChar' :: [Char] -> Bool
isAlphaNumChar' = all isAlphaNum