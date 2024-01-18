megold :: [Int] -> Int -> Int
megold osztok maxi = sum (filter (\x -> any (\m -> x `mod` m == 0) osztok) [1..maxi-1])

main :: IO ()
main = do
  let result = megold [3, 5] 20
  putStrLn $ "Osszeg: " ++ show result
