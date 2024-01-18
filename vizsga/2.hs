cartesianExp :: Int -> [a] -> [[a]]
cartesianExp 0 _ = [[]]
cartesianExp n x = [xs:y | xs <- x, y <- cartesianExp (n - 1) x]

main :: IO ()
main = do
    let szorzat = cartesianExp 3 "AZ"
    putStrLn $ "Az osszes eleme: " ++ show szorzat
