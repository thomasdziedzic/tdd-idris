import ReadNum
import System

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
  putStrLn ("You have made " ++ show guesses ++ " guesses.")
  Just guessNum <- readNumber | Nothing => do putStrLn "Invalid input"
                                              guess target (S guesses)
  case compare guessNum target of
       LT => do putStrLn "Your guess is too small :("
                guess target (S guesses)
       EQ => putStrLn "Your guess is correct!"
       GT => do putStrLn "Your guess is too big :("
                guess target (S guesses)

main : IO ()
main = do
  randomTime <- time
  let randomNum = mod randomTime 10
  guess (cast randomNum) 0

my_repl : (prompt : String) -> (onInput : String -> String) -> IO ()
my_repl prompt onInput = do
  putStr prompt
  input <- getLine
  let output = onInput input
  putStr output
  my_repl prompt onInput

my_replWith : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
my_replWith state prompt onInput = do
  putStr prompt
  input <- getLine
  case onInput state input of
       Nothing => pure ()
       Just (output, nextState) => do putStr output
                                      my_replWith nextState prompt onInput
