import Data.Vect

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} index xs = case integerToFin index n of
                     Nothing => Nothing
                     Just fIndex => Just (Vect.index fIndex xs)
