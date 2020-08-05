import Data.Vect

data StackCmd : Type -> Nat -> Nat -> Type where
  Push : Integer -> StackCmd () height (S height)
  Pop : StackCmd Integer (S height) height
  Top : StackCmd Integer (S height) (S height)

  Pure : ty -> StackCmd ty height height
  (>>=) : StackCmd a state1 state2 -> (a -> StackCmd b state2 state3) -> StackCmd b state1 state3

testAdd : StackCmd Integer 0 0
testAdd = do
  Push 10
  Push 20
  val1 <- Pop
  val2 <- Pop
  Pure (val1 + val2)

runStack : (stk : Vect inHeight Integer) -> StackCmd ty inHeight outHeight -> (ty, Vect outHeight Integer)
runStack stk (Push val) = ((), val :: stk)
runStack (val :: stk) Pop = (val, stk)
runStack (val :: stk) Top = (val, val :: stk)
runStack stk (Pure x) = (x, stk)
runStack stk (cmd >>= next) = let (cmdRes, newStk) = runStack stk cmd in
                             runStack newStk (next cmdRes)

doAdd : StackCmd () (S (S height)) (S height)
doAdd = do
  val1 <- Pop
  val2 <- Pop
  Push (val1 + val2)
