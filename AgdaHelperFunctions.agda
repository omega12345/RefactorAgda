-- stuff I did not find in the standard library, but which
-- I might just not have noticed.

module AgdaHelperFunctions where

infixr 0 _$_
_$_ : {A B : Set} -> (A -> B) -> A -> B
x $ y = x y
