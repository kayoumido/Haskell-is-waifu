module Lab5
where

import Data.Char

data Person = Person {
  fname   ::  String,
  lname   ::  String,
  age     ::  Int,
  height  ::  Float
} deriving (Show)

wam   = Person "Doran"    "Kayoumi" 24 183
jesus = Person "Nicolas"  "Mueller" 22 183

people = [wam, jesus]

initials (Person f l _ _)
    | null f    = "X. " ++ initial l
    | null l    = initial f ++ " X."
    | otherwise = initial f ++ ' ':initial l
    where
        initial w = toUpper (head w):['.']

search (Person f l _ _) =
    filter (\p -> fname p == f && lname p == l)

oldify = map (\(Person f l a h) -> succ a)

avgage l =
    let (a, b) = foldl (\(x, y) p -> (x + age p, y + 1)) (0, 0) l in
        fromIntegral a / fromIntegral b



data List a = Empty
    | Node a (List a)

instance Show t => Show (List t) where
    show Empty = "empty"
    show (Node a l) = show a ++ ':':show l

ins a Empty = Node a Empty
ins a l@(Node b bs)
    | a <= b = Node a l
    | otherwise = Node b (ins a bs)

del a Empty = Empty
del a l@(Node b bs)
    | a < b = l
    | a == b = bs
    | otherwise = Node b (del a bs)



data Tree a = Nil
    | Nod a (Tree a) (Tree a)
    deriving(Show, Read)

tins a Nil = Nod a Nil Nil
tins a t@(Nod x lhs rhs)
    | a == x = t
    | a < x = Nod x (tins a lhs) rhs
    | otherwise = Nod x lhs (tins a rhs)

tsearch _ Nil = False
tsearch a (Nod x lhs rhs)
    | a == x = True
    | a < x = tsearch a lhs
    | otherwise = tsearch a rhs

squash Nil = []
squash (Nod a Nil Nil) = [a]
squash (Nod a lhs rhs) = squash lhs ++ [a] ++ squash rhs