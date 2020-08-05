import Data.List.Views
import Data.Vect
import Data.Vect.Views
import Data.Nat.Views

total
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
  equalSuffix [] ys | Empty = []
  equalSuffix (xs ++ [x]) ys | (Snoc xsrec) with (snocList ys)
    equalSuffix (xs ++ [x]) [] | (Snoc xsrec) | Empty = []
    equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec) = if x == y
                                                                         then equalSuffix xs ys | xsrec | ysrec ++ [x]
                                                                         else []

total
mergeSort : Ord a => Vect n a -> Vect n a
mergeSort xs with (splitRec xs)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (left ++ right) | (SplitRecPair lrec rrec) = merge (mergeSort left | lrec) (mergeSort right | rrec)

total
toBinary : Nat -> String
toBinary n with (halfRec n)
  toBinary Z | HalfRecZ = ""
  toBinary (x + x) | (HalfRecEven rec) = toBinary x | rec ++ "0"
  toBinary (S (x + x)) | (HalfRecOdd rec) = toBinary x | rec ++ "1"

total
palindrome : List Char -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (ys ++ [y])) | (VCons rec) = x == y && palindrome ys | rec
