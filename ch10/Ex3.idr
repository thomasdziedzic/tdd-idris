import DataStore
import Shape_abs

getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues input with (storeView input)
  getValues input | SNil = []
  getValues (addToStore (key, value) store) | (SAdd rec) = value :: getValues store | rec

testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            empty

area : Shape -> Double
area s with (shapeView s)
  area (triangle base height) | STriangle = base * height * 0.5
  area (rectangle width height) | SRectangle = width * height
  area (circle radius) | SCircle = pi * radius * radius
