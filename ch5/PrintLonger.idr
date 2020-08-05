printLonger : IO ()
printLonger = do
  putStr "Enter the first string: "
  firstStr <- getLine
  putStr "Enter the second string: "
  secondStr <- getLine
  let longestLen = max (length firstStr) (length secondStr)
  putStrLn ("The length of the longest string is: " ++ show longestLen)

printLonger2 : IO ()
printLonger2 =
  putStr "Enter the first string: " >>= \_ =>
  getLine >>= \firstStr =>
  putStr "Enter the second string: " >>= \_ =>
  getLine >>= \secondStr  =>
  let longestLen = max (length firstStr) (length secondStr) in
  putStrLn ("The length of the longest string is: " ++ show longestLen)
