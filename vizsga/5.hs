{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use map with tuple-section" #-}


init_dict :: Enum a => a -> a -> [(a, Int)]
init_dict start end = zip [start..end] (repeat 0)

fill_dict :: Eq a => [(a, Int)] -> [a] -> [(a, Int)]
fill_dict dict [] = dict
fill_dict dict (x:xs) = fill_dict (updateDict dict x) xs
    where
        updateDict [] _ = []
        updateDict ((c, n):rest) char
            | c == char = (c, n+1) : updateDict rest char
            | otherwise = (c, n) : updateDict rest char

rend_gen :: [(Char, Int)] -> [Char]
rend_gen [] = []
rend_gen ((char, freq):rest) = replicate freq char ++ rend_gen rest

konytar_rend :: String -> String
konytar_rend str = rend_gen (fill_dict (init_dict 'a' 'z') str)

main :: IO ()
main = do
    let input = "almaafaalatt"
        output = konytar_rend input
    putStrLn $ "Input: " ++ input
    putStrLn $ "Output: " ++ output