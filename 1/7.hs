encode :: (Eq t1, Num t2) => [t1] -> [(t2, t1)]
encode [] = []
encode (x:xs) = encode' 1 x xs

encode' :: (Eq t1, Num t2) => t2 -> t1 -> [t1] -> [(t2, t1)]
encode' n x [] = [(n, x)]
encode' n x (y:ys)
    |x == y = encode' (n + 1) x ys
    |otherwise = (n, x) : encode' 1 y ys

main :: IO ()
main = do
  let inputList = ["a","a","a","c","c","b"]
  let result = encode inputList
  putStrLn $ "Kompakt formaban: " ++ show result