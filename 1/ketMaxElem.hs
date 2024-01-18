ketmax :: (Ord a, Num a) => [a] -> [a]

ketmax (elso:masodik:lista)
    | elso > masodik = ketmaxsz lista elso masodik
    | otherwise = ketmaxsz lista masodik elso

ketmaxsz :: Ord a => [a] -> a -> a -> [a]

ketmaxsz [] max1 max2 = [max1, max2]
ketmaxsz [l] max1 max2 = [max1, max2]
ketmaxsz (l:ls) max1 max2
    | (l > max1) && (l > max2) = ketmaxsz ls l max1
    | (l < max1) && (l > max2) = ketmaxsz ls max1 l
    | otherwise = ketmaxsz ls max1 max2

main :: IO ()
main = do
  let lista = [3, 8, 5, 12, 7, 9, 2, 1, 4, 11, 5]
  let eredmeny = ketmax lista
  putStrLn $ "A ket legnagyobb elem: " ++ show eredmeny