reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

isPalindrom :: Eq a => [a] -> Bool
isPalindrom xs = xs == reverseList xs

main :: IO ()
main = do
  let palindromLista = "lehel"
  let nemPalindromLista = "csato"

  putStrLn $ "Palindrom: " ++ show (isPalindrom palindromLista)
  putStrLn $ "Palindrom: " ++ show (isPalindrom nemPalindromLista)