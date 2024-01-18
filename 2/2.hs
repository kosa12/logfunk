import Data.Char (isLower, toLower)

gomb :: [String]
gomb = ["1", "aábc2", "deéf3", "ghií4", "jkl5", "mnoóöő6", "pqrs7", "wxyz9", "+ 0", ".,#"]

oldPhoneEllen :: [[Char]] -> [Char] -> Bool
oldPhoneEllen gomb = all (\c -> any (elem (toLower c)) gomb)

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


main :: IO ()
main = do
  let y = "Alma"
      alma = oldPhone y
  putStrLn $ show y ++ "= " ++ show alma

  let y = "AlmaAFan"
      alma2 = oldPhoneEllen gomb y
  putStrLn $ show y ++ "= " ++ show alma2

  let y = "Lehel! 2+4=6"
      alma2 = oldPhoneEllen gomb y
  putStrLn $ show y ++ "= " ++ show alma2


