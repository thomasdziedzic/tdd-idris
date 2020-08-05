import Data.Vect

%default total

vectTake : (toTake : (Fin (S n))) -> Vect n a -> Vect (cast toTake) a
vectTake FZ xs = []
vectTake (FS f) (x :: xs) = x :: vectTake f xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                Just idx => let x = Vect.index idx xs
                                                y = Vect.index idx ys in
                                                Just (x + y)
