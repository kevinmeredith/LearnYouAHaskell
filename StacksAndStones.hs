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