module Laboratoire_1
(
  impairs,
  pairs,
  carrés,
  paires,
  triplets,

  addition,
  carré,
  ou,
  et,
  test,
  
  somme,
  produit,
  sansHello,
  commenceParHello,
  pasDeHello,
  
  tuple,
  ages,
  age,
  age'
)

where
  
import Data.Char

-- Exercice 1.3
impairs n = [1,3..n]
pairs n = [i | i<-[1..n], even i]
carrés n = [i*i | i<-[1..n], i*i<100]
paires n =[(x,y) | x<-[1..n], y<-[1..n], x<y]
paires' n = let l=[1..n] in
  [(x,y) | x<-l, y<-l, x<y]
triplets n = let l = [1..n] in
  [(x,y,z)| x<-l, y<-l, z<-l, z*z==x*x+y*y]

-- Exercice 1.4
addition x y z = x + y + z
carré x = x^2
ou p q = if p then True else q
et p q = if p then q else False
test f = [(p,q,f p q) | p<-[False, True], q<-[False ..True]]
  
-- Exercice 1.5
somme liste = if null liste then 0 else head liste + (somme (tail liste))
produit liste = if null liste then 1 else head liste + (produit (tail liste))
sansHello s = drop 6 s
commenceParHello s = take 5 s == "Hello"
pasDeHello s = if commenceParHello s then
  (let r = sansHello s in toUpper (head r):(tail r)) else s
  
-- Exercice 1.6
tuple n = let sq x = x*x in (sq n, sq (n+1), sq (n+2))
ages = [("Marine",28),("Théo",26),("Chloé",23)]
age nom liste = 
  let t = head liste in
  if fst t == nom then snd t else age nom (tail liste)
  
age' nom liste = head [y | (x,y) <- liste, x==nom]
