import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Int)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

TupleVect : (n : Nat) -> (typeArg : Type)-> Type
TupleVect Z typeArg= ()
TupleVect (S k) typeArg = (typeArg, TupleVect k typeArg)

test : TupleVect 4 Nat
test = (1, 2, 3, 4, ())
