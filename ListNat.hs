{-# LANGUAGE GADTs #-}

module ListNat where

import Prelude hiding 
    ((+), (*), (^), fact, fib, double, min, max, (<), (<=), sum, length)

import Nat

type ListNat = [Nat]

length :: ListNat -> Nat
length [] = O
length (n:ns) = S(length ns)

sum :: ListNat -> Nat
sum [] = O
sum (n:ns) = n + sum ns