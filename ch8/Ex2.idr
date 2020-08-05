import Data.Vect

%default total

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = rewrite sym (plusZeroRightNeutral m) in
                             Refl
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in
                         rewrite plusSuccRightSucc m k in
                                 Refl

reverseProof_nil : Vect n a -> Vect (plus n 0) a
reverseProof_nil {n} xs = rewrite plusZeroRightNeutral n in xs

reverseProof_xs : Vect (S (n + m)) a -> Vect (n + (S m)) a
reverseProof_xs {n} {m} xs = rewrite sym (plusSuccRightSucc n m) in xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where
    reverse' : Vect n a -> Vect m a -> Vect (n + m) a
    reverse' acc [] = reverseProof_nil acc
    reverse' acc (x :: xs) = reverseProof_xs (reverse' (x::acc) xs)
