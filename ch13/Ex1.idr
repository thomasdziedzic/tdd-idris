data GuessCmd : Type -> Nat -> Nat -> Type where
  Try : Integer -> GuessCmd Ordering (S guesses) guesses

  Pure : ty -> GuessCmd ty state state
  (>>=) : GuessCmd a state1 state2 -> (a -> GuessCmd b state2 state3) -> GuessCmd b state1 state3

threeGuesses : GuessCmd () 3 0
threeGuesses = do
  Try 10
  Try 20
  Try 15
  Pure ()

{-
noGuesses : GuessCmd () 0 0
noGuesses = do
  Try 10
  Pure ()
-}

data Matter = Solid | Liquid | Gas

namespace Matter
  data MatterCmd : Type -> Matter -> Matter -> Type where
    Melt : MatterCmd () Solid Liquid
    Boil : MatterCmd () Liquid Gas
    Condense : MatterCmd () Gas Liquid
    Freeze : MatterCmd () Liquid Solid

    Pure : a -> MatterCmd a state state
    (>>=) : MatterCmd a state1 state2 -> (a -> MatterCmd b state2 state3) -> MatterCmd b state1 state3

iceSteam : MatterCmd () Solid Gas
iceSteam = do Melt
              Boil

steamIce : MatterCmd () Gas Solid
steamIce = do Condense
              Freeze

{-
overMelt : MatterCmd () Solid Gas
overMelt = do Melt
              Melt
-}
