module Labo2
(
  addVectors,
  scalarProduct,
  sum',
  product',
  head',
  tail',
  fifth',
  pgdc,
)
where

-- Exercice 2.1
-- 1)
-- 1. :t 3 * pi                     --> Float
-- 2. :t (1.5, "3")                 --> (a, [Char])
-- 3. :t head "Hello " ++ "World !" --> [Char]          --> ERROR
-- 4. :t [[1, 2], []]               --> [[Num]]
-- 5. :t [('a', 1), ('b', 2)]       --> Num b => [(Char, b)]

-- 2)
-- 1. [[[1]]]
-- 2. [(2, 'c')]
-- 3. (2.2, [[2]])
-- 4. (((2, 5), [True], 3.14))

-- Exercice 2.2
-- 1.
addVectors (a, b) (c, d) = (a + c, b + d)

-- 2.
scalarProduct (a, b, c) (x, y, z) = (a*x + a*y + a*z, b*x + b*y + b*z, c*x + c*y + c*z)

-- 3.
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' [] = 1
product' (x:xs) = x * product' xs

-- 4.
head' (x:_) = x
tail' (_:xs) = xs

-- 5.
fifth' (_:_:_:_:x:_) = x

-- Exercice 2.3
-- 1.
pgdc a b
  | a < b = pgdc a (b - a)
  | a > b = pgdc (a - b) b
  | a == b = a

-- 2.
toTime sec
  | sec < 60 = (0, 0, sec)
  | sec < 3600 = let (h, m, s) = toTime (sec - 60) in (h, m + 1, s)
  | sec >= 3600 = let (h, m, s) = toTime (sec - 3600) in (h + 1, m, s)