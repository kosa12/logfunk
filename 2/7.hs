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

main :: IO ()
main = do
  let tree = listaBolFa [4, 3, 1, 8]
  putStrLn $ "fa: " ++ show tree

  let newTree = beszur 2 tree
  putStrLn $ "Beszur: " ++ show newTree

  let deletedTree = torol 3 newTree
  putStrLn $ "Torol: " ++ show deletedTree
