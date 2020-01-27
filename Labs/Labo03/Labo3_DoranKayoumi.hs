-- Author: Doran Kayoumi
-- Date  : 06.10.2019

module Labo3 (
  ack,
  quotient,
  nieme,
  puissance,
  duplique,
  renverse,
  rotation,
  rotation',
  insertion,
  suppression,
  union,
  partage,
  fusion,
  triFusion,
)
where

ack (m, n)
  | m == 0          = n + 1
  | m > 0 && n == 0 = ack ((m-1), 1)
  | m > 0 && n > 0  = ack ((m-1), ack(m, (n-1)))

quotient n m 
  | m == 0    = error "0 Division"
  | m == n    = 1
  | m == 1    = n
  | otherwise = 1 + quotient (n-m) m 

nieme (x:xs) n
  | n == 1    = x
  | otherwise = nieme xs (n-1)

puissance x i
  | i == 0     = 1
  | i == 1     = x
  | otherwise  = x * puissance x (i-1)

duplique []     = []
duplique (x:xs) = x:x:duplique xs

renverse []     = []
renverse (x:xs) = renverse xs ++ [x]

rotation [] _ = []
rotation l i  = let x = take i l in drop i l ++ x

rotation' [] _      = []
rotation' l 0       = l
rotation' (x:xs) 1  = xs ++ [x] 
rotation' (x:xs) i  = rotation' (xs ++ [x]) (i-1) 

insertion [] n = [n]
insertion (x:xs) n
  | x /= n    = x:insertion xs n
  | otherwise = (x:xs)

suppression [] _ = []
suppression (x:xs) n
  | x /= n    = x:suppression xs n
  | otherwise = xs

union l []      = l
union l (y:ys)  = union (insertion l y) ys 

partage []  = ([], [])
partage [x] = ([x], [])
partage (x:y:l) =
  let (xs, ys) = partage l in (x:xs, y:ys)

fusion xs [] = xs
fusion [] ys = ys
fusion (x:xs) (y:ys)
  | x <= y    = x:fusion xs (y:ys)
  | otherwise = y:fusion (x:xs) ys

triFusion []  = []
triFusion [x] = [x]
triFusion l = 
  fusion (triFusion firstHalf) (triFusion secondHalf)
  where (firstHalf, secondHalf) = partage l
