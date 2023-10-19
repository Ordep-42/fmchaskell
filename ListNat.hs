{-# LANGUAGE GADTs #-}

module ListNat where

import Prelude hiding 
    ((+), (*), (^), fact, fib, double, min, max, (<), (<=), sum, length, product, elem, (++))

import Nat

type ListNat = [Nat]

length :: ListNat -> Nat
length [] = O
length (n:ns) = S(length ns)

elem :: Nat -> ListNat -> Bool
elem _ [] = False
elem n (m:ms) = n == m || elem n ms

sum :: ListNat -> Nat
sum [] = O
sum (n:ns) = n + sum ns

product :: ListNat -> Nat
product [] = (S O)
product (n:ns) = n * product ns

(++) :: ListNat -> ListNat -> ListNat
(++) [] ns = ns
(++) (n:ns) ms = (n:(++)ns ms)

enum :: Nat -> ListNat
enum O = (O:[])
enum (S n) = ((S n):enum n)
