-- copied from ch4
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle base height) = base * height
area (Circle radius) = pi * radius * radius

Eq Shape where
  (==) (Triangle base height) (Triangle base' height') = base == base' && height == height'
  (==) (Rectangle width height) (Rectangle width' height') = width == width && height == height'
  (==) (Circle radius) (Circle radius') = radius == radius'
  (==) _ _ = False

Ord Shape where
  compare x y = compare (area x) (area y)

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]
