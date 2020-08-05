data Vect : Nat -> Type -> Type where
  Nil : Vect 0 a
  (::) : (x : a) -> (xs : Vect n a) -> Vect (S n) a

Eq a => Eq (Vect n a) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys

Foldable (Vect n) where
  foldr f acc [] = acc
  foldr f acc (x :: xs) = f x (foldr f acc xs)
