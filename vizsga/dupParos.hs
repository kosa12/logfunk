doubleEven :: Integral a => [a] -> [a]
doubleEven [] = []
doubleEven (x:xs)
    | even x    = x * 2 : doubleEven xs
    | otherwise = doubleEven xs


main :: IO ()
main = do
  let inputList = [1, 2, 3, 4, 5]
      result = doubleEven inputList
  putStrLn $ "Input list: " ++ show inputList
  putStrLn $ "Doubled even elements: " ++ show result

