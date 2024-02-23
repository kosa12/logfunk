import GHC.ResponseFile (escapeArgs)
import System.Win32 (COORD(xPos))
-- kompaktaltbol nem az

decomp :: [(a,Int)] -> [a]
decomp [] = []
decomp ((ez,ennyiszer):es)
       | ennyiszer == 0 = decomp es
       | ennyiszer > 0 = ez : decomp ((ez,(ennyiszer - 1)):es)


decompMap:: [(a,Int)] -> [a]
decompMap lista  = concat $ map (\(a,b) -> replicate b a) lista 



--

count :: (Num a, Eq t) => t -> [t] -> a
count e [] = 1
count e (x:xs)
       | e == x = 1 + count e xs
       | otherwise = count e xs


hisztogram :: (Num b, Eq a) => [a] -> [(a, b)]
hisztogram [] = []
hisztogram (e:es) = (e,(count e es)):hisztogram (filter (/=e) es)


-- Fibo
fib :: Num t => t -> t -> [t]
fib a b = a : fib b (a + b) 



-- all cond list

alll :: (a -> Bool) -> [a] -> Bool
alll _ [] = True
alll cond (e:es)
       | cond e = alll cond es
       | otherwise = False


-- mapp

mapp :: (a -> b) -> [a] -> [b]
mapp _ [] = []
mapp fg (e:es) = fg e : mapp fg es


-- filtR 

filtR :: (a -> Bool) -> [a] -> [a]
filtR _ [] = []
filtR cond (e:es)
       | cond e = e : filtR cond es
       | otherwise = filtR cond es



-- foldR (1(2(3(4 + 0))))

foldR fg kezd [] = kezd
foldR fg kezd (e:es) = e `fg` foldR fg kezd es



-- foldL ()

foldL fg kezd [] = kezd
foldL fg kezd (e:es) = foldL fg (kezd `fg` e) es



-- (!!!)

kiszed :: Int -> [a] -> Maybe a
kiszed p [] = Nothing
kiszed p (e:es)
       | p == 0 = Just e
       | otherwise = kiszed (p - 1) es


(!!!) p lista = case kiszed p lista of
                     Just szam -> szam
                     Nothing -> -1



-- appaned ezt ehhez  

appaned :: [a] -> [a] -> [a]
appaned [] eztet = eztet
appaned (e:es) eztet  = e : appaned es eztet


appanedFoldR x s = foldr (:) s x




-- isorT

isorT :: (Ord a) => [a] -> [a]
isorT = foldr (beszur) []
beszur :: Ord t => t -> [t] -> [t]
beszur x [] = [x]
beszur x (e:es)
       | x > e = e : beszur x es
       | otherwise = x:e:es




-- halmazza alakit

halmazosit :: (Ord a) => [a] -> [a]
halmazosit [] = [] 
halmazosit (e:es) = e : halmazosit (filter (/=e) es)



--pontossag 64 biten 2/2^n == 0

pontossag :: Int
pontossag = until cond iter 0
       where
              cond n = 2/2^n == 0
              iter n = n + 1


-- e-nek az erteke

e :: Double
e = x where (_,x,_) = until (\(_,_,x) -> x==0) (\(k,sum,akt) -> (k + 1, sum + akt/(k + 1), akt/(k + 1))) (0,1,1)