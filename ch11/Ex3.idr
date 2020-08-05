import Data.Primitives.Views
import System

%default total

data Input = Cat String
           | Copy String String
           | Exit

data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String
  ReadFile : (filepath : String) -> Command (Either FileError String)
  WriteFile : (filepath : String) -> (contents : String) -> Command (Either FileError ())

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

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (ReadFile filepath) = readFile filepath
runCommand (WriteFile filepath contents) = writeFile filepath contents
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

parseInput : List String -> Maybe Input
parseInput ("cat" :: filename :: []) = Just (Cat filename)
parseInput ("copy" :: source :: destination :: []) = Just (Copy source destination)
parseInput ("exit" :: []) = Just Exit
parseInput _ = Nothing

readInput : (prompt : String) -> Command (Maybe Input)
readInput prompt = do PutStr prompt
                      answer <- GetLine
                      Pure (parseInput (words answer))

runInput : Input -> Command ()
runInput (Cat filepath) = do
  Right contents <- ReadFile filepath
    | Left fileError => PutStr (show fileError)
  PutStr contents
runInput (Copy source destination) = do
  Right sourceContents <- ReadFile source
    | Left fileError => PutStr (show fileError)
  Right _ <- WriteFile destination sourceContents
    | Left fileError => PutStr (show fileError)
  Pure ()
runInput Exit = Pure ()

shell : ConsoleIO ()
shell = do
  PutStr "Welcome to the matrix\n"
  input <- readInput "> "
  case input of
       Nothing => do PutStr "Invalid input!"
                     shell
       Just Exit => Quit ()
       Just input => do runInput input
                        shell

partial
main : IO ()
main = do run forever shell
          pure ()
