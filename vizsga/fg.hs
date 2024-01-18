f :: ([a1] -> [a2] -> [a3]) -> [a1] -> [a2] -> [[a3]]
f _ [] _ = []
f a b c = a (init b) (take (length (init b)) (init c)) : f a (tail b) (tail c)

--0 1 2 3
-- 10 15 20 25

-- 0 1 2 10 15 25

-- 1 2 15 20

-- 2 20
-- map sum $ f (++) [1..3] [100,200..]