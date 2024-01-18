fibo :: [Integer]
fibo = 1 : 1 : zipWith (+) fibo (tail fibo)

result30 :: [Integer]
result30 = take 30 fibo

result150 :: [Integer]
result150 = take 150 fibo

main :: IO ()
main = do
  putStrLn $ "30. Fibonacci: " ++ show result30
  putStrLn $ "150. Fibonacci: " ++ show result150