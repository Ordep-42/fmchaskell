module Bool where

import Prelude hiding (true, false, Bool)

data Bool = True | False
    deriving (Eq, Show)

if_then_else :: Bool -> Nat -> Nat -> Nat
if_then_else True n _ = n
if_then_else False _ m = m
