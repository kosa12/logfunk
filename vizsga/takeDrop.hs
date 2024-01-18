take1 :: Int -> [a] -> [a]
take1 n (x:xs)
    | n > 0 = x : take1 (n - 1) xs
take1 _ _ = []

drop1 :: Int -> [a] -> [a]
drop1 n (x:xs)
    | n > 0 = drop1 (n - 1) xs
drop1 _ xs = xs

main :: IO ()
main = do
    let lista = [1, 1, 2]
    let x = take1 1 lista
        y = drop1 1 lista
    putStrLn $ "Take result: " ++ show x
    putStrLn $ "Drop result: " ++ show y
    putStrLn $ "Ossz:" ++ show (x ++ y)