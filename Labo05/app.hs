module Labo5
where

import Data.Char

data Person = Person {
  fname   ::  String,
  lname   ::  String,
  age     ::  Int,
  height  ::  Float
} deriving (Show)

wam   = Person "Doran"    "Kayoumi" 24 183
jesus = Person "Nicolas"  "Mueller" 24 183

people = [wam, jesus]

initiales (Person f l _ _)
  | null f    = "X." ++ initial l
  | null l    = initial f ++ ".X"
  | otherwise = initial f ++ "." ++ initial l
  where initial c = [toUpper $ head c]

search (Person f l _ _) = filter (\person -> fname person == f && lname person == l)

oldify = map (\(Person f l a s) -> (Person f l (a+1) s))

averageage l = 
  let (x, y) = foldl (\(a, b) p -> (a + age p, b + 1)) (0, 0) l in 
    fromIntegral x / fromIntegral y



data List a = Empty | Node a (List a) 

instance (Show a) => Show (List a) where
  show Empty      = "Nothing"
  show (Node a l) = show a ++ ", " ++ show l

insert x Empty = Node x $ Empty
insert x node@(Node a l) = insert x l