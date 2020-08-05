module RemoveElem

import Data.Vect

{-
removeElem : DecEq a => (value : a) -> (xs : Vect (S n) a) -> Vect n a
removeElem value (x :: xs) = case decEq value x of
                                  Yes prf => xs
                                  No contra => x :: removeElem value xs

-}

oneInVector : Elem 1 [1,2,3]
oneInVector = Here

maryInVector : Elem "mary" ["peter", "paul", "mary"]
maryInVector = There (There Here)

export
removeElem : (value : a) -> (xs : Vect (S n) a) -> {auto prf : Elem value xs} -> Vect n a
removeElem value (value :: ys) {prf = Here} = ys
removeElem {n = Z} value (y :: []) {prf = There later} = absurd later
removeElem {n = (S k)} value (y :: ys) {prf = There later} = y :: removeElem value ys

{-
removeElem_auto : (value : a) -> (xs : Vect (S n) a) -> {auto prf : Elem value xs} -> Vect n a
removeElem_auto value xs {prf} = removeElem value xs prf
-}
