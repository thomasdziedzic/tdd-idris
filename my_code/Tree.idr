%default total

data Tree e = Empty
            | Node (Tree e) e (Tree e)

mirrorTree : Tree e -> Tree e
mirrorTree Empty = Empty
mirrorTree (Node l x r) = Node (mirrorTree r) x (mirrorTree l)

mirrorTreeInvolution : (t : Tree e) -> (t = mirrorTree (mirrorTree t))
mirrorTreeInvolution Empty = Refl
mirrorTreeInvolution (Node l x r) = let lPrf = mirrorTreeInvolution l
                                        rPrf = mirrorTreeInvolution r in
                                        rewrite lPrf in
                                        rewrite rPrf in
                                                Refl

prfEmptyNode : (Empty = Node l x r) -> Void
prfEmptyNode Refl impossible

prfNodeEmpty : (Node l x r = Empty) -> Void
prfNodeEmpty Refl impossible

elemUnequal : ((x = x') -> Void) -> (Node l x r = Node l' x' r') -> Void
elemUnequal f Refl = f Refl

lTreeUnequal : ((l = l') -> Void) -> (Node l x r = Node l' x' r') -> Void
lTreeUnequal f Refl = f Refl

rTreeUnequal : ((r = r') -> Void) -> (Node l x r = Node l' x' r') -> Void
rTreeUnequal f Refl = f Refl

DecEq a => DecEq (Tree a) where
  decEq Empty Empty = Yes Refl
  decEq Empty (Node l x r) = No prfEmptyNode
  decEq (Node l x r) Empty = No prfNodeEmpty
  decEq (Node l x r) (Node l' x' r') = case decEq x x' of
                                            Yes prfX => case decEq l l' of
                                                             Yes prfL => case decEq r r' of
                                                                              Yes prfR => rewrite prfX in
                                                                                          rewrite prfL in
                                                                                          rewrite prfR in
                                                                                                  Yes Refl
                                                                              No contraR => No (rTreeUnequal contraR)
                                                             No contraL => No (lTreeUnequal contraL)
                                            No contraX => No (elemUnequal contraX)
