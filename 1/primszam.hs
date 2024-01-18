kiszur :: Integral a => a -> [a] -> [a]
kiszur _ [] = []
kiszur n (x:xs)
  | x `mod` n == 0 = kiszur n xs
  | otherwise = x : kiszur n xs

szita :: Integral a => [a] -> [a]
szita [] = []
szita (x:xs) = x : szita (kiszur x xs)

valaszt :: Integral a => Int -> a
valaszt n = szita [2..] !! (n - 1)

main :: IO ()
main = do
  let n = 10
  putStrLn $ "A " ++ show n ++ "-dik primszam: " ++ show (valaszt n)