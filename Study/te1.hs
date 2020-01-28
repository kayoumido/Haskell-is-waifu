-- Q1
-- l = [21, 45, 37, 27, 81]

-- pairs [] = []
-- pairs [a] = []
-- pairs (_:y:xs) = y:pairs xs

-- impairs [] = []
-- impairs [a] = [a]
-- impairs (x:_:xs) = x:impairs xs

-- -- Q2
-- --- 1
-- ins n [] = [n]
-- ins n l@(x:xs)
--     | n <= x = n:l
--     | otherwise ins n xs

-- l = [5,1,3,2,15,7]

--- 2
-- tri [] = []
-- tri l@(x:xs) =

-- Q3
-- paires = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]

-- predicate (a,b) = a==b
-- filtre _ _ [] = []
-- filtre _ 0 _ = []
-- filtre p n l@(x:xs)
--     | p x = x:filtre p (n-1) xs
--     | otherwise = filtre p n xs
