%default total

data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = do
  putStr prompt
  input <- getLine
  let output = action input
  putStr output
  totalREPL prompt action

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> InfIO -> IO ()
run (More fuel) (Do action cont) = do
  result <- action
  run fuel (cont result)
run Dry p = putStrLn "Out of fuel"

partial
forever : Fuel
forever = More forever
