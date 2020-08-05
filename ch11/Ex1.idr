import Data.Primitives.Views

import Arith

every_other : Stream a -> Stream a
every_other (v1 :: v2 :: xs) = v2 :: every_other xs

data Face = Heads
          | Tails

getFace : Int -> Face
getFace x with (divides x 2)
  getFace ((2 * div) + rem) | (DivBy prf) = if rem == 0
                                               then Heads
                                               else Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips count xs = map getFace (take count xs)

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number initialApprox = iterate (\approx => (approx + (number / approx)) / 2) initialApprox

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double
square_root_bound Z number bound (value :: approxs) = value
square_root_bound (S k) number bound (value :: approxs) = if (abs (number - (value * value))) < bound
                                                             then value
                                                             else square_root_bound k number bound approxs

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001 (square_root_approx number number)
