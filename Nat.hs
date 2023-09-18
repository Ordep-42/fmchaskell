module Nat where

import Prelude hiding 
    ((+), (*), (^), fact, fib, double, min, max)

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

