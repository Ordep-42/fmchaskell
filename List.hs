module List where

import Prelude hiding
    (map)

import Bool

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x:map f xs