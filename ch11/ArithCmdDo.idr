import Data.Primitives.Views
import System

data Input = Answer Int
           | QuitCmd

data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String

  Pure : ty -> Command ty
  Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

readInput : (prompt : String) -> Command Input
readInput prompt = do PutStr prompt
                      answer <- GetLine
                      if toLower answer == "quit"
                         then Pure QuitCmd
                         else Pure (Answer (cast answer))

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure val) = pure val
runCommand (Bind c f) = do res <- runCommand c
                           runCommand (f res)

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit value) = do pure (Just value)
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)
run Dry p = pure Nothing

partial
forever : Fuel
forever = More forever

mutual
  correct : Stream Int -> (score : Nat) -> (totalQuestions : Nat) -> ConsoleIO (Nat, Nat)
  correct nums score totalQuestions = do PutStr "Correct!\n"
                                         quiz nums (score + 1) totalQuestions

  wrong : Stream Int -> Int -> (score : Nat) -> (totalQuestions : Nat) -> ConsoleIO (Nat, Nat)
  wrong nums ans score totalQuestions = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
                                           quiz nums score totalQuestions

  quiz : Stream Int -> (score : Nat) -> (totalQuestions : Nat) -> ConsoleIO (Nat, Nat)
  quiz (num1 :: num2 :: nums) score totalQuestions= do
    PutStr ("Score so far: " ++ show score ++ " / " ++ show totalQuestions ++ "\n")
    input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
    case input of
         Answer answer => if answer == num1 * num2
                             then correct nums score (S totalQuestions)
                             else wrong nums (num1 * num2) score (S totalQuestions)
         QuitCmd => Quit (score, totalQuestions)

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound num with (divides num 12)
      bound ((12 * div) + rem) | (DivBy prf) = rem + 1

partial
main : IO ()
main = do
  seed <- time
  Just (score, totalQuestions) <- run forever (quiz (arithInputs (fromInteger seed)) 0 0)
    | Nothing => putStrLn "Ran out of fuel"
  putStrLn ("Final score: " ++ show score ++ " / " ++ show totalQuestions)
