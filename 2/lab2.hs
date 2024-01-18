
import Data.Char (isLower, toLower)

kodol :: Int -> Int -> [Char] -> String
kodol m c input =
  let kiszurt = filter (/= ' ') (map toLower input)
      darabolt = darabol m kiszurt
      oszlopok = oszlopokkent darabolt
      osszerakott = concat oszlopok
      darabolt2 =  darabol c osszerakott
      eredmeny = darabolt2
  in unwords eredmeny

darabol :: Int -> [a] -> [[a]]
darabol _ [] = []
darabol m input = take m input : darabol m (drop m input)

oszlopokkent :: [[a]] -> [[a]]
oszlopokkent input
  | all null input = []
  | otherwise = map head input : oszlopokkent (filter (not . null) (map tail input))


gomb :: [String]
gomb = ["1", "aábc2", "deéf3", "ghií4", "jkl5", "mnoóöő6", "pqrs7", "wxyz9", "+ 0", ".,#"]

oldPhoneMain :: [[Char]] -> [Char] -> Bool
oldPhoneMain gomb = all (\c -> any (elem (toLower c)) gomb)

oldPhone :: [Char] -> [(Char, Int)]
oldPhone [] = []
oldPhone (s:ss)
  | isLower s = (char (keres gomb s 1), keresInd (gombInd gomb s) s 1) : oldPhone ss
  | otherwise = ('*', 1) : oldPhone (toLower s : ss)

keres :: [String] -> Char -> Integer -> Integer
keres (l:ls) a k
  | elem (toLower a) l = k
  | otherwise = keres ls a (k + 1)

keresInd :: String -> Char -> Int -> Int
keresInd (l:ls) a k
  | toLower l == toLower a = k
  | otherwise = keresInd ls a (k + 1)

gombInd :: [String] -> Char -> String
gombInd (k:ks) a
  | toLower a `elem` k = k
  | otherwise = gombInd ks a

char :: Integer -> Char
char a
  | a <= 10 = toEnum (fromEnum '0' + fromInteger a)
  | otherwise = '#'

binaryPrecision :: Double
binaryPrecision = until (\x -> 2 ** (-x) == 0) (+1) 0


ln :: Double -> Double
ln x
    | abs x >= 1 = -ln2 (1 / x)
    | otherwise = ln2 x

ln2 :: Double -> Double
ln2 x2 = lnx
    where
        (_, lnx, _) = until (\(x, y, z) -> abs (y - x) == 0) (sorbafejtes x2) (x2, 0, 1)

sorbafejtes :: Double -> (Double, Double, Int) -> (Double, Double, Int)
sorbafejtes x2 (x, y, k) = (y, y + (-1) ^ (k - 1) * ((x2 - 1) ^ k) / fromIntegral k, k + 1)


cumulOp :: (a -> a -> a) -> [a] -> [a]
cumulOp _ [] = []
cumulOp op (x:xs) = x : megold 1 x xs
  where
    megold _ _  [] = []
    megold k kov  (x:xs) = kov `op` x : megold (k + 1) (kov `op` x) xs


data Complex = Complex { real :: Double, imag :: Double }

instance Show Complex where
  show :: Complex -> String
  show (Complex a b)
    | b < 0     = show a ++ show b ++ "i" 
    | otherwise = show a ++ "+" ++ show b ++ "i"

instance Num Complex where
  (+) :: Complex -> Complex -> Complex
  (Complex a b) + (Complex c d) = Complex (a + c) (b + d)
  
  (-) :: Complex -> Complex -> Complex
  (Complex a b) - (Complex c d) = Complex (a - c) (b - d)
  
  (*) :: Complex -> Complex -> Complex
  (Complex a b) * (Complex c d) = Complex (a * c - b * d) (a * d + b * c)
  
  abs :: Complex -> Complex
  abs (Complex a b) = Complex (sqrt (a*a + b*b)) 0

instance Fractional Complex where
  (/) :: Complex -> Complex -> Complex
  (Complex a b) / (Complex c d) = Complex ((a*c + b*d) / (c*c + d*d)) ((b*c - a*d) / (c*c + d*d))



data BinFa a =
   Nodus (BinFa a) a (BinFa a)
   | Level

instance (Show a) => Show (BinFa a) where
  show :: Show a => BinFa a -> String
  show Level = "Level"
  show (Nodus bal elem jobb) = "Nodus (" ++ show bal ++ ") " ++ show elem ++ " (" ++ show jobb ++ ")"


beszur :: (Ord a) => a -> BinFa a -> BinFa a
beszur x Level = Nodus Level x Level
beszur x (Nodus bal elem jobb)
  | x < elem   = Nodus (beszur x bal) elem jobb
  | x > elem   = Nodus bal elem (beszur x jobb)
  | otherwise  = Nodus bal elem jobb


listaBolFa :: (Ord a) => [a] -> BinFa a
listaBolFa = foldr beszur Level


torol :: (Ord a) => a -> BinFa a -> Maybe (BinFa a)
torol _ Level = Nothing
torol x (Nodus bal elem jobb)
  | x < elem   = (\newBal -> Nodus newBal elem jobb) <$> torol x bal
  | x > elem   = (\newJobb -> Nodus bal elem newJobb) <$> torol x jobb
  | otherwise  = case (bal, jobb) of
                    (Level, _) -> Just jobb
                    (_, Level) -> Just bal
                    (_, _)     -> let minElem = minimumElem jobb
                                   in (\newJobb -> Nodus bal minElem newJobb) <$> torol minElem jobb
  where
    minimumElem :: BinFa a -> a
    minimumElem (Nodus Level elem _) = elem
    minimumElem (Nodus bal _ _)      = minimumElem bal