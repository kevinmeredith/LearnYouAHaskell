import Control.Monad.State

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push x ys = ((), x : ys)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let 
   ((),newStack1) = push 3 stack
   (a ,newStack2) = pop newStack1
   in pop newStack2

stackManip2 :: Stack -> (Int, Stack)
stackManip2 = do
  push 3
  a <- pop
  pop   

popStateMonad :: State Stack Int 
popStateMonad = state $ \(x:xs) -> (x, xs)

pushStateMonad :: Int -> State Stack ()
pushStateMonad a = state $ \xs -> ((), a:xs)

stackManipStateMonad :: State Stack Int 
stackManipStateMonad = do
	pushStateMonad 3 
	a <- popStateMonad
	popStateMonad

stackStuff :: State Stack ()
stackStuff = do
  a <- popStateMonad
  if a == 5
     then pushStateMonad 5
     else do
       pushStateMonad 3
       pushStateMonad 8

moreStack :: State Stack ()
moreStack = do
  a <- stackManipStateMonad
  if a == 100
    then stackStuff
    else return ()       		

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1,2,3]
     then put [8,3,1]
     else put [9,2,1]    