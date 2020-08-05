import Data.Vect

total my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = S (my_length xs)

total my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x]

total my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

total my_map_v : (a -> b) -> Vect n a -> Vect n b
my_map_v f [] = []
my_map_v f (x :: xs) = f x :: my_map_v f xs
