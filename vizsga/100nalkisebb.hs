num100alatt :: (Num a, Ord a) => [a] -> Maybe a
num100alatt [] = Nothing
num100alatt xs
    | null filteredList = Nothing
    | otherwise         = Just (foldr max (head filteredList) filteredList)
    where
        filteredList = filter (< 100) xs

main :: IO ()
main = do
  let inputList2 = [99, 100, 98]
      result2 = num100alatt inputList2
  putStrLn $ "Maximum below 100: " ++ show result2



