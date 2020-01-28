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
