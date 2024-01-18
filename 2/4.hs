ln :: Double -> Double
ln x
    | abs x >= 1 = -ln2 (1 / x)
    | otherwise = ln2 x

ln2 :: Double -> Double
ln2 x2 = lnx
    where
        (_, lnx, _) = until (\(x, y, z) -> abs (y - x) == 0) (sorbafejtes x2) (x2, 0, 1)

sorbafejtes :: Double -> (Double, Double, Int) -> (Double, Double, Int)
sorbafejtes x2 (x, y, k) = (y, y + (-1) ^ (k - 1) * ((x2 - 1) ^ k) / fromIntegral k, k + 1)

main :: IO ()
main = do
  let y = 2.5
      approx = ln y
  putStrLn $ "ln(" ++ show y ++ ") = " ++ show approx