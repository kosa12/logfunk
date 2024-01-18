fibo :: (Num t1, Num a, Ord t1) => t1 -> a
fibo 0 = 1
fibo 1 = 1
fibo n = fibo_ n 1 1

fibo_ :: (Ord t1, Num t1, Num t2) => t1 -> t2 -> t2 -> t2
fibo_ k egy ketto 
    | k < 2 = ketto
    | otherwise = fibo_ (k-1) ketto (egy+ketto)

result30 :: Integer
result30 = fibo 30

result150 :: Integer
result150 = fibo 150


main :: IO ()
main = do
  putStrLn $ "30. Fibonacci: " ++ show result30
  putStrLn $ "150. Fibonacci: " ++ show result150
