-- Caesar cipher - shift characters by n
import Data.Char (ord, chr)

encode :: Int -> String -> String
encode n = map $ shift' n
                 where shift' x =  chr . (+ x) . ord

decode :: Int -> String -> String
decode n = map $ shift' n
                 where shift' x =  chr . abs . ((-) x) . ord