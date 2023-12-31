module Bool where

import Prelude hiding (True, False, Bool)

data Bool = True | False
    deriving (Eq, Show)

if_then_else :: Bool -> a -> a -> a
if_then_else True n _ = n
if_then_else False _ m = m
