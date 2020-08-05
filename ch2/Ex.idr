module Main

palindrome : Nat -> String -> Bool
palindrome n str = let lowerStr = toLower str in
                     (lowerStr == reverse lowerStr) && (length lowerStr > n)

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten : Ord a => List a -> List a
top_ten xs = take 10 (reverse (sort xs))

over_length : Nat -> List String -> Nat
over_length n xs = length (filter (> n) (map length xs))
