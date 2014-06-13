-- LYAH sequence
main = do
  rs <- sequence [getLine, getLine, getLine]
  print rs

--> main
--foo
--bar
--baz
--["foo","bar","baz"]