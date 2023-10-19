module Nat where

import Prelude hiding 
    ((+), (*), (^), fact, fib, double, min, max, (<), (<=))

data Nat = O | S Nat
    deriving (Eq, Show)

(+) :: Nat -> Nat -> Nat
(+) n O = n
(+) n (S m) = S (n + m)

(*) :: Nat -> Nat -> Nat
(*) n O = O
(*) n (S m) = n + (n * m)

(^) :: Nat -> Nat -> Nat
(^) n O = S O
(^) n (S m) = n * (n ^ m)

double :: Nat -> Nat
double O = O
double (S n) = S ( S (double n))

fact :: Nat -> Nat
fact O = S O
fact (S n) = S n * fact n

fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S(S n)) = fib (S n) + fib n

min :: Nat -> Nat -> Nat
min (S n) O = O
min O (S n) = O
min (S n) (S m) = S (min n m)

max :: Nat -> Nat -> Nat
max (S n) O = S n
max O (S n) = S n
max (S n) (S m) = S (max n m)

od :: Nat -> Bool
od O = False
od (S O) = True
od (S(S n)) = od n

ev :: Nat -> Bool
ev O = True
ev (S O) = False
ev (S(S n)) = ev n

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

leq :: Nat -> Nat -> Bool
leq O _ = True
leq _ O = False
leq (S n) (S m) = leq n m
