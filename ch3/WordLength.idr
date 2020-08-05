allLengths : List String -> List Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

{-
isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)
-}

mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k
