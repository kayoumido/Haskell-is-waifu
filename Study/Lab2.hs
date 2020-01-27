module Lab2
where

import Data.Char

sumVects (a, b) (c, d) =
    (a+c, b+d)

scalar (a, b, c) (x, y, z) =
    (a*x + a*y + a*z, b*x + b*y + b*z, c*x + c*y + c*z)

sum' [] = 0
sum' (x:xs) =
    x + sum' xs

mpy [] = 1
mpy (x:xs) =
    x * mpy xs

head' (x:_) = x
tail' (_:xs) = xs

fifth (_:_:_:_:x:_) = x

gcd' a b
    | a > b = gcd' (a-b) b
    | a < b = gcd' a (b-a)
    | otherwise = a

toTime sec
    | sec < 60    = (0, 0, sec)
    | sec < 3600  = let (h, m, s) = toTime (sec - 60) in (h, 1+m, s)
    | otherwise   = let (h, m, s) = toTime (sec - 3600) in (1+h, m, s)

initials f l
    | null f    = "X. " ++ initial l
    | null l    = initial f ++ " X."
    | otherwise = initial f ++ ' ':initial l
    where
        initial w = toUpper (head w):['.']

factor n = split n 2
    where
        split 1 _ = [1]
        split n f
            | mod n f == 0 = f:split (div n f) f
            | otherwise = split n (f+1)

nth l n =
    show (l !! (n-1)) ++ " is the " ++
    case n of
        1 -> "first"
        2 -> "second"
        n -> show n ++ "th"
    ++ " element of the list"

take' n l =
    case (n, l) of
        (0, _)      -> []
        (_, [])     -> []
        (n, (x:xs)) -> x:take' (n-1) xs