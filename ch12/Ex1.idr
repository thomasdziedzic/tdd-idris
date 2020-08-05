import Control.Monad.State

import public TreeLabelState

update : (stateType -> stateType) -> State stateType()
update f = do
  x <- get
  put (f x)

increase : Nat -> State Nat ()
increase x = update (+x)

countEmpty : Tree a -> State Nat ()
countEmpty Empty = do
  increase 1
countEmpty (Node left val right) = do
  countEmpty left
  countEmpty right

increaseEmpty : State (Nat, Nat) ()
increaseEmpty = do
  (empty, node) <- get
  put (empty + 1, node)

increaseNode : State (Nat, Nat) ()
increaseNode = do
  (empty, node) <- get
  put (empty, node + 1)

countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = increaseEmpty
countEmptyNode (Node left val right) = do
  countEmptyNode left
  increaseNode
  countEmptyNode right
