osszefesul :: Ord a => [a] -> [a] -> [a]
osszefesul [] ys = ys
osszefesul xs [] = xs
osszefesul (x:xs) (y:ys)
    | x <= y    = x : osszefesul xs (y:ys)
    | otherwise = y : osszefesul (x:xs) ys

main :: IO ()
main = do
  let lista1 = [1, 3, 5, 7, 9]
  let lista2 = [2, 4, 6, 8, 10]
  let eredmeny = osszefesul lista1 lista2
  putStrLn $ "Az osszefesult lista: " ++ show eredmeny