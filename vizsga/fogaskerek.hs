type Coord = (Float, Float)
type Radius = Float
type Gear = (Coord, Radius)
type Gears = [Gear]

rosszul :: Gears -> Bool
rosszul gears = any (\(gear1, radius1) -> any (\(gear2, radius2) -> not (gear1 == gear2) && distance gear1 gear2 < radius1 + radius2) gears) gears
  where
    distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

touches :: Gears -> Gear -> Gears
touches gears (coord, radius) = filter (\(_, r) -> distance coord r <= radius) gears
  where
    distance (x1, y1) r = sqrt (x1^2 + y1^2) - r



main :: IO ()
main = do
  let gears = [((0, 0), 2), ((4, 0), 2), ((8, 0), 2)]
      selectedGear = ((4, 0), 2)
      touchingGears = touches gears selectedGear
  putStrLn $ "Selected Gear: " ++ show selectedGear
  putStrLn $ "Touching Gears: " ++ show touchingGears
