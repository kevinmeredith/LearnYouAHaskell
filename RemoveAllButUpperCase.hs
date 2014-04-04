let isUpperCase x = if (x >= 'A' && x <= 'Z') then True else False
let keepIfPredicate p x e = if(p) then x else e
let removeAllButUpperCase xs = [a | a <- xs, isUpperCase a]

-- test should return "HELLOWORLD"
removeAllButUpperCase "hiHELLO worldWORLD"
