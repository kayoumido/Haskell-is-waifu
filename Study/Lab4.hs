module Lab4
where

check f = map (\(x,y) -> (x, y, f x y))

f l n = map (\fn -> fn n) l

lists = map (:[])

square = map (^2)

listify = map (\x -> [1..x])

negitify = map (\x -> if x < 0 then 0 else x)
negitify' = map $ max 0

godd = filter odd
geven = filter even

filterGT n = filter (>n)
filterLT n = filter (<n)

take5String = map (\s -> take 5 s)

existe l pred = foldl (\acc x -> acc || pred x) False l

tous l pred = foldl (\acc x -> acc && pred x) True l

collatz 1 = [1]
collatz n
    | odd n = n:collatz (n*3+1)
    | even n = n:collatz (n `div` 2)
