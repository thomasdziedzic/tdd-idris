import Data.Vect

readToBlank : IO (List String)
readToBlank = do
  x <- getLine
  case x == "" of
       True => pure []
       False => do rest <- readToBlank
                   pure (x :: rest)

contents : (input : List String) -> String
contents [] = "\n"
contents (x :: xs) = x ++ "\n" ++ contents xs

readAndSave : IO ()
readAndSave = do
  input <- readToBlank
  putStrLn "Please enter a filename: "
  filename <- getLine
  Right () <- writeFile filename (contents input) | Left err => putStrLn ("Got error while writing to file (" ++ filename ++ "): " ++ show err)
  pure ()

readVectHandle : (file : File) -> IO (n ** Vect n String)
readVectHandle file = do
  isEOF <- fEOF file
  case isEOF of
       True => pure (_ ** [])
       False => do Right line <- fGetLine file | Left err => pure (_ ** [])
                   (len ** vec) <- readVectHandle file
                   pure (S len ** line :: vec)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  -- open file
  Right h <- openFile filename Read | Left err => pure (_ ** [])

  -- get all lines from the file
  result <- readVectHandle h

  -- close file
  closeFile h

  pure result
