module Lab1
where

import Data.Char

nodd n = [1,3..n]
neven n = [x | x <- [1..n], even x]
perfect n = [x*x | x <- [1..n], x*x < n]
tuple n = let l = [1..n] in
    [(x,y) | x <- l, y <- l, x < y]
tripule n = let l = [1..n] in
    [(x,y,z) | x <- l, y <- l, z <- l, x*x + y*y == z*z]

add x y z = x + y + z
square x = x**2
tripule' n = let l = [1..n] in
    [(x,y,z) | x <- l, y <- l, z <- l, (square x) + (square y) == square z]

or' p q = if p then True else q
and' p q = if p then q else False
test f = let l = [True, False] in
    [(x, y, f x y) | x <- l, y <- l]

toLowerString = map toLower

sanitize s = let w = "hello " in
    if (map toLower $ take (length w) s) == w then
        let res = drop (length w) s in toUpper (head res):(tail res)
    else
        w

truple n =
    let sq x = x*x in (sq n, sq (n+1), sq (n+2))

peeps = [("Doran", 24), ("Yasmine", 24), ("Nico", 22)]

getAge name ((n,a):xs)
    | name == n = a
    | otherwise = getAge name xs