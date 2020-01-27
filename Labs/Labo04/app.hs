module Labo4 (
  check,
  check',
  f,
  listes,
  listes',
  carres,
  carres',
  carres'',
  carres''',
  something,
  replaceNegative,
  replaceNegative',
  replaceNegative'',
  filterEven,
  filterOdd,
  filterGT,
  filterLT,
  filterTake,
  filterTake',
  cherche,
  existe,
  existe',
  existe'',
  existe''',
  collatz,
) where

-- 4.1
check f l = map (\(a, b) -> (a, b, a `f` b)) l
--- Better version
check' f = map (\(a, b) -> (a, b, a `f` b))

f l x = map (\func -> func x) l

listes l  = map (:[]) l
listes'   = map (:[])

-- 4.2
carres    l = map (^2) l
carres'   l = map (\a -> a^2) l
carres''  l = map (flip (^) 2) l
carres'''   = map (^2)

something l = map (\a -> [1..a]) l

replaceNegative   l = map (\a -> if a < 0 then 0 else a) l
replaceNegative'  l = map (max 0) l
replaceNegative''   = map $ max 0

-- 4.3
filterEven  = filter even
filterOdd   = filter odd

filterGT l x = filter (>x) l
filterLT l x = filter (<x) l

filterTake l = map (take 5) l
filterTake' = map $ take 5

cherche p = head . filter p

-- 4.3
existe      l p = foldl (\r x -> r || p x) False l
existe'     l p = foldl (||) False (map p l)
existe''    p l = foldl (||) False (map p l)
existe'''   p   = foldl (||) False . map p


-- 4.5
collatz 1 = [1]
collatz n 
  | odd   n = n:collatz (n*3+1)
  | even  n = n:collatz (n `div` 2)