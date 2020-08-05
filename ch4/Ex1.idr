import Tree
import Shape
import Picture

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = treeToList left ++ [x] ++ treeToList right

data Expr = Single Int
          | Addition Expr Expr
          | Subtraction Expr Expr
          | Multiplication Expr Expr

evaluate : Expr -> Int
evaluate (Single x) = x
evaluate (Addition x y) = evaluate x + evaluate y
evaluate (Subtraction x y) = evaluate x - evaluate y
evaluate (Multiplication x y) = evaluate x * evaluate y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe orig@(Just x) Nothing = orig
maxMaybe (Just x) (Just y) = Just (max x y)

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive triangle@(Triangle x y)) = Just (area triangle)
biggestTriangle (Primitive other) = Nothing
biggestTriangle (Combine pic pic1) = maxMaybe (biggestTriangle pic) (biggestTriangle pic1)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
