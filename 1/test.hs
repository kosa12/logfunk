--Kosa Matyas, 522

fibo :: (Num t1, Num a, Ord t1) => t1 -> a
fibo 0 = 1
fibo 1 = 1
fibo n = fibo_ n 1 1

fibo_ :: (Ord t1, Num t1, Num t2) => t1 -> t2 -> t2 -> t2
fibo_ k egy ketto 
    | k < 2 = ketto
    | otherwise = fibo_ (k-1) ketto (egy+ketto)

result30 :: Integer
result30 = fibo 30

result150 :: Integer
result150 = fibo 150

fibo2 :: [Integer]
fibo2 = 1 : 1 : zipWith (+) fibo2 (tail fibo2)

result302 :: [Integer]
result302 = take 30 fibo2

result1502 :: [Integer]
result1502 = take 150 fibo2

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


osszefesul :: Ord a => [a] -> [a] -> [a]
osszefesul [] ys = ys
osszefesul xs [] = xs
osszefesul (x:xs) (y:ys)
    | x <= y    = x : osszefesul xs (y:ys)
    | otherwise = y : osszefesul (x:xs) ys

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

isPalindrom :: Eq a => [a] -> Bool
isPalindrom xs = xs == reverseList xs


deleteEveryKth :: Int -> [a] -> [a]
deleteEveryKth k = deleteEveryKth_ k 1

deleteEveryKth_ :: Int -> Int -> [a] -> [a]
deleteEveryKth_ _ _ [] = []
deleteEveryKth_ k count (x:xs)
  | count `mod` k == 0 = deleteEveryKth_ k (count + 1) xs
  | otherwise = x : deleteEveryKth_ k (count + 1) xs


encode :: (Eq t1, Num t2) => [t1] -> [(t2, t1)]
encode [] = []
encode (x:xs) = encode' 1 x xs

encode' :: (Eq t1, Num t2) => t2 -> t1 -> [t1] -> [(t2, t1)]
encode' n x [] = [(n, x)]
encode' n x (y:ys)
    |x == y = encode' (n + 1) x ys
    |otherwise = (n, x) : encode' 1 y ys

kiszur :: Integral a => a -> [a] -> [a]
kiszur _ [] = []
kiszur n (x:xs)
  | x `mod` n == 0 = kiszur n xs
  | otherwise = x : kiszur n xs

szita :: Integral a => [a] -> [a]
szita [] = []
szita (x:xs) = x : szita (kiszur x xs)

valaszt :: Integral a => Int -> a
valaszt n = szita [2..] !! (n - 1)

megold :: [Int] -> Int -> Int
megold osztok maxi = sum (filter (\x -> any (\m -> x `mod` m == 0) osztok) [1..maxi-1])