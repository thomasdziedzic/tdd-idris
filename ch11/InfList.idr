module InfList

public export
data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

Functor InfList where
  map func (value :: xs) = func value :: map func xs

%name InfList xs, ys, zs

countFrom : Integer -> InfList Integer
countFrom x = x :: countFrom (x + 1)

getPrefix : (count : Nat) -> InfList ty -> List ty
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs
