{-# LANGUAGE GADTs #-}

module ListNat where

import Prelude hiding 
    ((+), (*), (^), fact, fib, double, min, max, (<), (<=), sum, length, product, (++))

import Nat

type ListNat = [Nat]

length :: ListNat -> Nat
length [] = O
length (n:ns) = S(length ns)

sum :: ListNat -> Nat
sum [] = O
sum (n:ns) = n + sum ns

product :: ListNat -> Nat
product [] = (S O)
product (n:ns) = n * product ns

(++) :: ListNat -> ListNat -> ListNat
(++) [] ns = ns
(++) (n:ns) ms = (n:(++)ns ms)
