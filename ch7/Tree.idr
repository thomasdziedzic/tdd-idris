data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Functor Tree where
  map func Empty = Empty
  map func (Node l x r) = Node (map func l) (func x) (map func r)

Foldable Tree where
  foldr func acc Empty = acc
  foldr func acc (Node l x r) = let leftfold = foldr func acc left
                                    rightfold = foldr func leftfold right in
                                    func e rightfold
