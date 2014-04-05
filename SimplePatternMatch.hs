-- simple pattern match from "Learn You a Haskell"

sayMe:: (Integral a) => a -> String
sayMe 1 = "one!"
sayMe 2 = "two!"
sayMe 3 = "three!"
sayMe 4 = "four!"
sayMe 5 = "five!!!"
sayMe x = "none..."