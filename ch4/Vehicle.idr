module Vehicle

%default total

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
  Unicycle : Vehicle Pedal
  Bicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  ElectricCar : (charge : Nat) -> Vehicle Electric
  Tram : (charge : Nat) -> Vehicle Electric

wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels Motorcycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (ElectricCar charge) = 4
wheels (Tram charge) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
