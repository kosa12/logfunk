cumulOp :: (a -> a -> a) -> [a] -> [a]
cumulOp _ [] = []
cumulOp op (x:xs) = x : megold 1 x xs
  where
    megold _ _  [] = []
    megold k kov  (x:xs) = kov `op` x : megold (k + 1) (kov `op` x) xs


main :: IO ()
main = do
  let example1 = cumulOp (+) [2, 4, 3, 4, 3]
      example2 = cumulOp (++) ["a", "l", "m", "a"]
  putStrLn $ "+: " ++ show example1
  putStrLn $ "++: " ++ show example2