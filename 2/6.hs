data Complex = Complex { real :: Double, imag :: Double }

instance Show Complex where
  show :: Complex -> String
  show (Complex a b)
    | b < 0     = show a ++ show b ++ "i" 
    | otherwise = show a ++ "+" ++ show b ++ "i"

instance Num Complex where
  (+) :: Complex -> Complex -> Complex
  (Complex a b) + (Complex c d) = Complex (a + c) (b + d)
  
  (-) :: Complex -> Complex -> Complex
  (Complex a b) - (Complex c d) = Complex (a - c) (b - d)
  
  (*) :: Complex -> Complex -> Complex
  (Complex a b) * (Complex c d) = Complex (a * c - b * d) (a * d + b * c)
  
  abs :: Complex -> Complex
  abs (Complex a b) = Complex (sqrt (a*a + b*b)) 0

instance Fractional Complex where
  (/) :: Complex -> Complex -> Complex
  (Complex a b) / (Complex c d) = Complex ((a*c + b*d) / (c*c + d*d)) ((b*c - a*d) / (c*c + d*d))

main :: IO ()
main = do
  let a = Complex 2 3
  let b = Complex (-1) 1
  putStrLn $ "a = " ++ show a
  putStrLn $ "b = " ++ show b
  putStrLn $ "a + b = " ++ show (a + b)
  putStrLn $ "a - b = " ++ show (a - b)
  putStrLn $ "a * b = " ++ show (a * b)
  putStrLn $ "a / b = " ++ show (a / b)
  putStrLn $ "abs a = " ++ show (abs a)
  putStrLn $ "abs b = " ++ show (abs b)