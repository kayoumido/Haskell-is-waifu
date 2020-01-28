module Lab6
where

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

symsym Nil = True
symsym (Nod a Nil Nil) = True
symsym (Nod a lhs rhs) = mirror lhs rhs
    where   mirror Nil Nil  = True
            mirror _ Nil    = False
            mirror Nil _    = False
            mirror (Nod _ l1 r1) (Nod _ l2 r2) = (mirror l1 r2) && (mirror r1 l2)