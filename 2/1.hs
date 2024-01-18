import Data.Char (toLower)

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


main :: IO ()
main = do
  let input = "Alma a Fa aLatt"
  let m=5
  let c=5
  let result = kodol m c input
  putStrLn result
