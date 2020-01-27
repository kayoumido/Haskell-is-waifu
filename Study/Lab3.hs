module Lab3
where

ack m n
    | m == 0 = n + 1
    | m > 0 && n == 0 = ack (m-1) 1
    | m > 0 && n > 0  = ack (m-1) (ack m (n-1))

quotient _ 0 = error $ "0 division..you a bad boy!"
quotient 0 _ = 0
quotient n m
    | n < m     = 0
    | n == m    = 1
    | m == 1    = n
    | otherwise = 1 + quotient (n-m) m

nth (x:xs) n
    | n == 1    = x
    | otherwise = nth xs (n-1)

pow _ 0 = 1
pow x i = x * pow x (i-1)

copy [] = []
copy (x:xs) = x:x:copy xs

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

rotato [] _ = []
rotato l 0  = l
rotato (x:xs) 1  = xs ++ [x]
rotato (x:xs) i = rotato (xs ++ [x]) (i-1)

rotato' [] _ = []
rotato' l 0   = l
rotato' l i = let ll = take i l in drop i l ++ ll

ins [] y = [y]
ins l@(x:xs) y
    | x /= y = x:ins xs y
    | otherwise = l

del [] _ = []
del (x:xs) y
    | x == y = xs
    | otherwise = x:del xs y

union l [] = l
union l (y:ys) = union (ins l y) ys

split [] = ([], [])
split [x] = ([x], [])
split (x:y:xs) =
    (x:l1, y:l2)
    where
        (l1, l2) = split xs

merge [] [] = []
merge [] l  = l
merge l []  = l
merge l1@(x:xs) l2@(y:ys)
    | x < y = x:merge xs l2
    | otherwise = y:merge l1 ys

mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge (mergeSort l1) (mergeSort l2)
    where (l1, l2) = split l