data ElemL : a -> List a -> Type where
  HereL : ElemL x (x :: xs)
  ThereL : (later : ElemL x xs) -> ElemL x (y :: xs)

data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

Uninhabited (Last [] value) where
  uninhabited LastOne impossible
  uninhabited (LastCons _) impossible

notInNil : Last [] value -> Void
notInNil LastOne impossible
notInNil (LastCons _) impossible

notLast : (prfNotLast : (x = value) -> Void) -> Last [x] value -> Void
notLast prfNotLast LastOne = prfNotLast Refl
notLast prfNotLast (LastCons prf) = absurd prf

notInTail : (contra : Last (y :: xs) value -> Void) -> Last (x :: (y :: xs)) value -> Void
notInTail contra (LastCons LastOne) = contra LastOne
notInTail contra (LastCons (LastCons prf)) = contra (LastCons prf)

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notInNil
isLast (x :: []) value = case decEq x value of
                              Yes Refl => Yes LastOne
                              No prfNotLast => No (notLast prfNotLast)
isLast (x :: (y :: xs)) value = case isLast (y :: xs) value of
                                     Yes prf => Yes (LastCons prf)
                                     No contra => No (notInTail contra)
