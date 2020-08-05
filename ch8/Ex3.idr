%default total

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

%name Vect xs, ys

headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

prfList : {xs : Vect n a} -> {ys : Vect n a} -> (x = y) -> (xs = ys) -> (x :: xs) = (y :: ys)
prfList prfHead prfTail = rewrite prfHead in
                          rewrite prfTail in
                                  Refl

DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) = case decEq x y of
                                   Yes prfHead => case decEq xs ys of
                                                        Yes prfTail => Yes (prfList prfHead prfTail)
                                                        No contraTail => No (tailUnequal contraTail)
                                   No contraHead => No (headUnequal contraHead)
