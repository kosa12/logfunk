binaryPrecision :: Double
binaryPrecision = until (\x -> 2 ** (-x) == 0) (+1) 0

main :: IO ()
main = do
  print binaryPrecision