module Matrix

import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

{-
transposeHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect k elem)) -> Vect n (Vect (S k) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys
-}

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             zipWith (::) x xsTrans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

dotProduct : Num a => Vect n a -> Vect n a -> a
dotProduct [] [] = 0
dotProduct (x :: xs) (y :: ys) = x * y + dotProduct xs ys

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix xs ys = let ysTrans = transposeMat ys in
                       multMatrixHelper xs ysTrans
  where
    multMatrixHelper : Num a => Vect n (Vect m a) -> Vect p (Vect m a) -> Vect n (Vect p a)
    multMatrixHelper [] ys = []
    multMatrixHelper (x :: xs) ys = map (dotProduct x) ys :: multMatrixHelper xs ys
