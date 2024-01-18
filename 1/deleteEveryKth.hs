deleteEveryKth :: Int -> [a] -> [a]
deleteEveryKth k = deleteEveryKth_ k 1

deleteEveryKth_ :: Int -> Int -> [a] -> [a]
deleteEveryKth_ _ _ [] = []
deleteEveryKth_ k count (x:xs)
  | count `mod` k == 0 = deleteEveryKth_ k (count + 1) xs
  | otherwise = x : deleteEveryKth_ k (count + 1) xs

main :: IO ()
main = do
  let inputList = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let k = 3
  let result = deleteEveryKth k inputList
  putStrLn $ "Lista minden " ++ show k ++ "-adik eleme torolve: " ++ show result