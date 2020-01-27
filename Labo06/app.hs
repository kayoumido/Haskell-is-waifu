module Labo6
where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- instance (Show a) => Show (Tree a) where
--   show Empty          = "Nothing"
--   show (Node a t1 t2) = show a ++ "(" ++ show t1 ++ ", " ++ show t2 ++ ")"

insert a Empty = Node a Empty Empty
insert n t@(Node a t1 t2) 
  | n == a    = t
  | n < a     = Node a (insert n t1) t2
  | otherwise = Node a t1 (insert n t2)

contains n Empty = False
contains n (Node a t1 t2)
  | n == a    = True
  | n < a     = contains n t1
  | otherwise = contains n t2

squash Empty          = []
squash (Node a t1 t2) = a : (squash t1) ++ (squash t2)

symmetrical Empty        = True
symmetrical (Node _ x y) = mirror x y
  where mirror Empty Empty = True
        mirror Empty _     = False
        mirror _ Empty     = False
        mirror (Node _ g1 d1) (Node _ g2 d2) = (mirror g1 d2) && (mirror d1 g2)