import Data.Vect

data StackCmd : Type -> Nat -> Nat -> Type where
  Push : Integer -> StackCmd () height (S height)
  Pop : StackCmd Integer (S height) height
  Top : StackCmd Integer (S height) (S height)

  GetStr : StackCmd String height height
  PutStr : String -> StackCmd () height height

  Pure : ty -> StackCmd ty height height
  (>>=) : StackCmd a state1 state2 -> (a -> StackCmd b state2 state3) -> StackCmd b state1 state3

runStack : (stk : Vect inHeight Integer) -> StackCmd ty inHeight outHeight -> IO (ty, Vect outHeight Integer)
runStack stk (Push val) = pure ((), val :: stk)
runStack (val :: stk) Pop = pure (val, stk)
runStack (val :: stk) Top = pure (val, val :: stk)
runStack stk GetStr = do x <- getLine
                         pure (x, stk)
runStack stk (PutStr x) = do putStr x
                             pure ((), stk)
runStack stk (Pure x) = pure (x, stk)
runStack stk (x >>= f) = do (x', newStk) <- runStack stk x
                            runStack newStk (f x')

data StackIO : Nat -> Type where
  Do : StackCmd a height1 height2 -> (a -> Inf (StackIO height2)) -> StackIO height1

namespace StackDo
  (>>=) : StackCmd a height1 height2 -> (a -> Inf (StackIO height2)) -> StackIO height1
  (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> Vect height Integer -> StackIO height -> IO ()
run (More fuel) stk (Do c f) = do
  (res, newStk) <- runStack stk c
  run fuel newStk (f res)
run Dry stk p = pure ()

data StkInput = Number Integer
              | Add
              | Subtract
              | Multiply
              | Negate
              | Discard
              | Duplicate

doAdd : StackCmd () (S (S height)) (S height)
doAdd = do
  val1 <- Pop
  val2 <- Pop
  Push (val1 + val2)

doSubtract : StackCmd () (S (S height)) (S height)
doSubtract = do
  val1 <- Pop
  val2 <- Pop
  Push (val2 - val1)

doMultiply : StackCmd () (S (S height)) (S height)
doMultiply = do
  val1 <- Pop
  val2 <- Pop
  Push (val1 * val2)

doNegate : StackCmd () (S height) (S height)
doNegate = do
  val <- Pop
  Push (-val)

doDiscard : StackCmd Integer (S height) height
doDiscard = do
  val <- Pop
  Pure val

doDuplicate : StackCmd () (S height) (S (S height))
doDuplicate = do
  val <- Top
  Push val

strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "add" = Just Add
strToInput "subtract" = Just Subtract
strToInput "multiply" = Just Multiply
strToInput "negate" = Just Negate
strToInput "discard" = Just Discard
strToInput "duplicate" = Just Duplicate
strToInput x = if all isDigit (unpack x)
                  then Just (Number (cast x))
                  else Nothing

mutual
  tryAdd : StackIO height
  tryAdd {height = S (S h)} = do doAdd
                                 result <- Top
                                 PutStr (show result ++ "\n")
                                 stackCalc
  tryAdd = do PutStr "Fewer than two items on the stack\n"
              stackCalc

  trySubtract : StackIO height
  trySubtract {height = S (S h)} = do doSubtract
                                      result <- Top
                                      PutStr (show result ++ "\n")
                                      stackCalc
  trySubtract = do PutStr "Fewer than two items on the stack\n"
                   stackCalc

  tryMultiply : StackIO height
  tryMultiply {height = S (S h)} = do doMultiply
                                      result <- Top
                                      PutStr (show result ++ "\n")
                                      stackCalc
  tryMultiply = do PutStr "Fewer than two items on the stack\n"
                   stackCalc

  tryNegate : StackIO height
  tryNegate {height = S h} = do doNegate
                                result <- Top
                                PutStr (show result ++ "\n")
                                stackCalc
  tryNegate = do PutStr "No items on the stack\n"
                 stackCalc

  tryDiscard : StackIO height
  tryDiscard {height = S h} = do discardedVal <- doDiscard
                                 PutStr ("Discarded " ++ show discardedVal ++ "\n")
                                 stackCalc
  tryDiscard = do PutStr "No items on the stack\n"
                  stackCalc

  tryDuplicate : StackIO height
  tryDuplicate {height = S h} = do doDuplicate
                                   result <- Top
                                   PutStr ("Duplicated " ++ show result ++ "\n")
                                   stackCalc
  tryDuplicate = do PutStr "No items on the stack\n"
                    stackCalc

  stackCalc : StackIO height
  stackCalc = do PutStr "> "
                 input <- GetStr
                 case strToInput input of
                      Nothing => do PutStr "Invalid input \n"
                                    stackCalc
                      Just (Number x) => do Push x
                                            stackCalc
                      Just Add => tryAdd
                      Just Subtract => trySubtract
                      Just Multiply => tryMultiply
                      Just Negate => tryNegate
                      Just Discard => tryDiscard
                      Just Duplicate => tryDuplicate

partial
main : IO ()
main = run forever [] stackCalc
